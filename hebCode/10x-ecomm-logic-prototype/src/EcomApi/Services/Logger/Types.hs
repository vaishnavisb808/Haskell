{-|
    Module : EcomApi.Services.Logger.Types
    Description : Types specifically used for logging
-}
{-# LANGUAGE DeriveGeneric #-}


module EcomApi.Services.Logger.Types where

import           Data.Aeson
import           GHC.Generics

-- | Loglevels supported by the logging module
data LogLevel = DEBUG
              | INFO
              | WARN
              | ERROR deriving ( Show , Eq , Ord , Generic )

-- | to be able to parse LogLevel from config.json
instance FromJSON LogLevel

-- | logger type provides four functions one for each loglevel to facilitate
-- logging
data Logger = Logger
    { logDebug :: String -> IO()
    , logInfo  :: String -> IO()
    , logWarn  :: String -> IO()
    , logError :: String -> IO()
    }

-- | A logmessage alogn with it's loglevel used to push logs to channel
data Log = Log
    { logLevel   :: LogLevel
    , logMessage :: String
    }


-- | Used to specify the different destinations for the log messages
data LogType = Console
             | File deriving ( Show , Generic )

-- | to be able to parse from  config.json
instance FromJSON  LogType

