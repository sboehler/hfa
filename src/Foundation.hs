{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Import.NoFoundation
import Text.Jasmine (minifym)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

data App = App
  { appSettings :: AppSettings
  , appStatic :: Static -- ^ Settings for static file serving.
  , appHttpManager :: Manager
  , appLogger :: Logger
  , pool :: Pool Connection
  }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
  approot =
    ApprootRequest $ \app req ->
      fromMaybe
        (getApprootText guessApproot app req)
        (appRoot $ appSettings app)
  makeSessionBackend _ =
    Just <$> defaultClientSessionBackend 120 "config/client_session_key.aes"
  yesodMiddleware = defaultYesodMiddleware
  isAuthorized FaviconR _ = return Authorized
  isAuthorized RobotsR _ = return Authorized
  isAuthorized _ _ = return Authorized
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
  shouldLog app _source level =
    appShouldLogAll (appSettings app) ||
    level == LevelWarn || level == LevelError
  makeLogger = return . appLogger

instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
