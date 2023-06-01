{-|
    Module : ShopperAPI.Core.Config.Types
    Description : Types used for loading configuration from environment
    Types used for loading configuration from environment
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module           ShopperAPI.Core.Config.Types where

import           Control.Concurrent                 (Chan)
import           Data.Aeson                         (FromJSON)
import           Data.ByteString.Lazy               (ByteString)
import           Data.UUID                          (UUID)
import           GHC.Generics                       (Generic)
import           ShopperAPI.Services.Database.Types 
import           ShopperAPI.Services.Logger.Types   (Log, LogLevel, LogType)


-- | name of the config file
configFile    = "config.json"

-- | name of the env var to look for if fetching from file fails
configVarName = "CONFIG"

-- | config information required
data Configuration = Configuration
    { dbName     :: String
    , dbUser     :: String
    , dbPass     :: String
    , dbHost     :: String
    , logLevel   :: LogLevel
    , logTypes   :: [LogType]
    , authKey    :: String
    , serverPort :: Int
    } deriving (Generic,Show)

instance FromJSON Configuration

-- | Shared Environement. Type shared across all handlers
data Env = Env
    { envConfig     :: Configuration
    , envDbOps      :: DbOps
    , envLogChannel :: Chan Log
    , envRequestId  :: Maybe UUID
    }
