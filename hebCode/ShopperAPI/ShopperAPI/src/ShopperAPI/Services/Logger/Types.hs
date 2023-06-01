{-|
    Module : EcomApi.Services.Logger.Types
    Description : Types specifically used for logging
-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}



module           ShopperApi.Services.Logger.Types where

import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.UUID                         (UUID)
import           ShopperApi.Api.DataTypes
import           GHC.Generics                      (Generic)

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
    { runLog :: Log -> IO ()
    }


-- | Used to specify the different destinations for the log messages
data LogType = Console
             | File
             deriving ( Show , Generic )

-- | to be able to parse from  config.json
instance FromJSON  LogType


quickLog logLevel msg = Log logLevel
                            msg
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            Nothing

data HttpMethod = GET|POST deriving (Show,Eq,Generic)

-- | A logmessage alogn with it's loglevel used to push logs to channel
data Log = Log
            { logLevel            :: LogLevel
            , logMessage          :: String
            , endpoint            :: Maybe Endpoint
            , request_method      :: Maybe HttpMethod
            , request_status_code :: Maybe Integer
            , request_id          :: Maybe UUID
            , user                :: Maybe AccessData
            }
         | EventLog
            { logLevel     :: LogLevel
            , logMessage   :: String
            , eventName    :: String
            , eventOutCome :: String
            }

data Endpoint = UserSignUp deriving (Generic,Show)  --add your logging type here

data JsonLogUrl = JsonLogUrl
    { path :: Maybe String
    } deriving (Generic)

instance ToJSON JsonLogUrl

data JsonLogHttp = JsonLogHttp
    { method      :: Maybe String
    , status_code :: Maybe Integer
    , url_details :: Maybe JsonLogUrl
    } deriving (Generic)

instance ToJSON JsonLogHttp

data JsonLog = JsonLog
    { status  :: String
    , message :: String
    , http    :: JsonLogHttp
    } deriving (Generic)
instance ToJSON JsonLog

data JsonEventLog = JsonEventLog
    { status  :: String
    , message :: String
    , evt     :: JsonLogEvent
    } deriving (Generic)
instance ToJSON JsonEventLog

data JsonLogEvent = JsonLogEvent {
                                   name    :: String
                                 , outcome :: String
                                 } deriving (Show,Generic)
instance ToJSON JsonLogEvent