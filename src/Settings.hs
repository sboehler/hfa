{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Data.Aeson ((.!=), (.:?), withObject)
import Network.Wai.Handler.Warp (HostPreference)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
  { appStaticDir :: String
    -- ^ Directory from which to serve static files.
  , appRoot :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
  , appHost :: HostPreference
    -- ^ Host/interface the server should bind to.
  , appPort :: Int
    -- ^ Port to listen on
  , appIpFromHeader :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
  , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
  , appShouldLogAll :: Bool
    -- ^ Should all log messages be displayed?
  , appReloadTemplates :: Bool
    -- ^ Use the reload version of templates
  , appMutableStatic :: Bool
    -- ^ Assume that files in the static dir may change after compilation
  , appSkipCombining :: Bool
    -- ^ Perform no stylesheet/script combining
    -- Example app-specific configuration values.
  , appCopyright :: Text
    -- ^ Copyright text to appear in the footer of the page
  , appAnalytics :: Maybe Text
    -- ^ Google Analytics code
  , appPostgresqlHost :: Text
  , appPostgresqlPort :: Int
  , appPostgresqlDatabase :: Text
  , appPostgresqlUser :: Text
  , appPostgresqlPassword :: Text
  }

instance FromJSON AppSettings where
  parseJSON =
    withObject "AppSettings" $ \o -> do
#ifdef DEVELOPMENT
      let defaultDev = True
#else
      let defaultDev = False
#endif
      appStaticDir <- o .: "static-dir"
      appRoot <- o .:? "approot"
      appHost <- fromString <$> o .: "host"
      appPort <- o .: "port"
      appIpFromHeader <- o .: "ip-from-header"
      appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
      appShouldLogAll <- o .:? "should-log-all" .!= defaultDev
      appReloadTemplates <- o .:? "reload-templates" .!= defaultDev
      appMutableStatic <- o .:? "mutable-static" .!= defaultDev
      appSkipCombining <- o .:? "skip-combining" .!= defaultDev
      appCopyright <- o .: "copyright"
      appAnalytics <- o .:? "analytics"
      appPostgresqlHost <- o .: "postgresql-host"
      appPostgresqlPort <- o .: "postgresql-port"
      appPostgresqlDatabase <- o .: "postgresql-database"
      appPostgresqlUser <- o .: "postgresql-user"
      appPostgresqlPassword <- o .: "postgresql-password"
      return AppSettings {..}

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def
