{-# LANGUAGE DeriveGeneric#-}
module LoggerTypes where


import GHC.Generics
import Data.Aeson

data LogType = File|Console deriving (Show,Eq,Generic)
instance FromJSON LogType 


data LogLevel = DEBUG|INFO|WARN|ERROR deriving (Show,Eq,Generic)
instance FromJSON LogLevel
instance Ord LogLevel where
    compare ERROR ERROR = EQ
    compare ERROR _ = GT
    compare WARN ERROR = LT
    compare WARN WARN = EQ
    compare WARN _ = GT
    compare INFO INFO = EQ
    compare INFO ERROR = LT
    compare INFO WARN = LT
    compare INFO _ = GT
    compare DEBUG DEBUG = EQ
    compare DEBUG _ = LT