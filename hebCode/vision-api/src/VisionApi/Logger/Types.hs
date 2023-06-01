{-# LANGUAGE DeriveGeneric #-}
module VisionApi.Logger.Types where


import Data.Aeson
import GHC.Generics

-- | LogType specifies whether the output should be written to file or Printed to console
data LogType = File|Console deriving (Show,Eq,Generic)
instance FromJSON LogType

-- | LogLevel specifies priority of log messages and allows for filtering
data LogLevel = DEBUG|INFO|WARN|ERROR deriving (Show,Eq,Generic,Ord)

instance FromJSON LogLevel

