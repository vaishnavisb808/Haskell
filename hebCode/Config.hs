{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Config where

import GHC.Generics
import System.Environment
import Data.Either
import qualified Data.Aeson as A
import  qualified Data.ByteString.Lazy.Internal as B
import Data.ByteString.Lazy.UTF8
import Data.ByteString.Lazy as BSL
import Control.Monad.IO.Class
import System.Directory
import Data.ByteString.Lazy.Char8 as BSL8

instance A.FromJSON Configuration
--instance ToJSON Configuration

data Configuration= Configuration{
    dbName::String,
    dbUser::String,
    dbPassword::String
} deriving (Show,Generic) 

configFile = "config.json"
configVarName = "CONFIG"

getConfig::IO (Either String Configuration)
getConfig = do
    -- try reading config from config file
    configFromFile' <- getConfigFromFile configFile
    case configFromFile' of 
        (Right config) -> return $Right config
        (Left errmessage)-> do
            print errmessage
            -- upon failure attempt to read from env var
            print "attempting to obtain config from env var"
            configFromEnv' <- getConfigFromEnv configVarName
            case configFromEnv' of
                Right config -> return $Right config
                Left errmessage -> return $ Left errmessage

getConfigFromFile::String->IO (Either String Configuration)
getConfigFromFile  filePath = do
    fileExists <- liftIO $doesFileExist filePath
    if fileExists
        then do
            fileData <- BSL.readFile filePath
            let decoded' = A.decode fileData :: Maybe Configuration
            case decoded' of
                (Just decoded) -> return $Right decoded
                Nothing -> return $Left $"Could not parse "++configFile
        else return $Left "Could load config from file"


getConfigFromEnv:: String ->IO(Either String Configuration)
getConfigFromEnv envVar= do
    var<- lookupEnv envVar
    case var of
     Just var' -> do
         let val= A.decode (B.packChars var'):: Maybe Configuration 
         case val of 
             Just val'-> return $ Right val'
             Nothing -> return $Left $"Can't parse"-- ++envVar
     Nothing -> return $Left $"Can't find variable"-- ++ envVar
-- $env:config ='{ "dbName":"postgres","dbUser":"postgres","dbPassword":"1234"}'