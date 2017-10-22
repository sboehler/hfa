{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( getApplicationDev
  , appMain
  , develMain
  , makeFoundation
  , makeLogWare
    -- * for DevelMain
  , getApplicationRepl
  , shutdownApp
    -- * for GHCI
  , handler
  ) where

import Control.Monad.Logger (liftLoc)
import qualified Data.Text as T
import Import
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
       (Settings, defaultSettings, defaultShouldDisplayException, getPort,
        runSettings, setHost, setOnException, setPort)
import Network.Wai.Middleware.RequestLogger
       (Destination(Logger), IPAddrSource(..), OutputFormat(..),
        destination, mkRequestLogger, outputFormat)
import System.Log.FastLogger
       (defaultBufSize, newStdoutLoggerSet, toLogStr)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Common
import Handler.Home

import Data.Pool (Pool, createPool, withResource)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.Migration as PGM
       (runMigration, MigrationCommand(MigrationDirectory, MigrationInitialization),
        MigrationContext(..))

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

makeDatabasePool :: AppSettings -> IO (Pool PG.Connection)
makeDatabasePool settings = do
  let info =
        PG.defaultConnectInfo
        { PG.connectHost = T.unpack $ appPostgresqlHost settings
        , PG.connectPort = fromIntegral $ appPostgresqlPort settings
        , PG.connectUser = T.unpack $ appPostgresqlUser settings
        , PG.connectPassword = T.unpack $ appPostgresqlPassword settings
        , PG.connectDatabase = T.unpack $ appPostgresqlDatabase settings
        }
  pool <- createPool (PG.connect info) PG.close 1 10 10
  withResource pool $ \con  -> PG.withTransaction con $ do
      _ <- PGM.runMigration $ PGM.MigrationContext PGM.MigrationInitialization True con
      _ <- PGM.runMigration
        (PGM.MigrationContext (PGM.MigrationDirectory "config/migrations") True con)
      return ()
  return pool

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
 = do
  appHttpManager <- newManager
  appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic <-
    (if appMutableStatic appSettings
       then staticDevel
       else static)
      ("static")
    -- Return the foundation
  pool <- makeDatabasePool appSettings
  return App {..}

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
    { outputFormat =
        if appDetailedRequestLogging $ appSettings foundation
          then Detailed True
          else Apache
                 (if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket)
    , destination = Logger $ loggerSet $ appLogger foundation
    }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation) $
  setHost (appHost $ appSettings foundation) $
  setOnException
    (\_req e ->
       when (defaultShouldDisplayException e) $
       messageLoggerSource
         foundation
         (appLogger foundation)
         $(qLocation >>= liftLoc)
         "yesod"
         LevelError
         (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

getData :: IO (App, Settings, Application)
getData = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (foundation, wsettings, app)

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  (_, wsettings, app) <- getData
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain
    -- Get the settings from all relevant sources
 = do
  settings <-
    loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
      []
        -- allow environment variables to override
      useEnv
    -- Generate the foundation from the settings
  foundation <- makeFoundation settings
    -- Generate a WAI Application from the foundation
  app <- makeApplication foundation
    -- Run the application with Warp
  runSettings (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  (foundation, wsettings, appl) <- getData
  return (getPort wsettings, foundation, appl)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------
-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h
