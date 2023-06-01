{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module ShopperAPI.Core.Config.Types where

import      GHC.Generics
import      Data.Aeson
import      ShopperAPI.Database.Types
import      Servant
import      Control.Monad.Trans.Reader

-- name of the config file
configFile = "config.json"

-- name of the env var to look for if fetching from file fails
configVarName = "CONFIG"

-- | config information required
data Configuration = Configuration
    { dbName     :: String
    , dbUser     :: String
    , dbPass     :: String
    , dbHost     :: String
    , authKey    :: String
    , serverPort :: Int
    } deriving (Generic,Show)


instance FromJSON Configuration

-- | Shared Environement. Type shared across all handlers
data Env = Env
    { envConfig     :: Configuration
    , envDbOps      :: DbOps
    }