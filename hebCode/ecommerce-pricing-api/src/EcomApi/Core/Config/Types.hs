{-# LANGUAGE DeriveGeneric #-}

module EcomApi.Core.Config.Types(Configuration(..)) where

import EcomApi.Services.Logger.Types
import Data.Aeson
import GHC.Generics

data Configuration = Configuration
    { 
      logType      :: ![LogType]
    , logLevel     :: !LogLevel
    } deriving (Generic,Show)

instance FromJSON Configuration