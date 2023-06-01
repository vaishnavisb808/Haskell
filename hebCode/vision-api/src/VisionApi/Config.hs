{-
    Config Module
    exports a getConfgi function to read from a config.json at root and
    returns a configuration type.
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module VisionApi.Config
    ( getConfig
    , Configuration(..)
    , ExecEnv(..)
    ) where

import System.Directory
import System.Environment
import GHC.Generics
import Data.Aeson
import Control.Monad.IO.Class
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSL8
import VisionApi.Logger.Types

-- name of the config file
configFile = "config.json"

-- name of the env var to look for if fetching from file fails
configVarName = "CONFIG"

{- | ExecEnv is used to specify which environment the server is running in
     Used in scearios like enabling swagger docs only in dev environment
-}
data ExecEnv = DEV|STAGE|PROD deriving (Show,Eq,Generic)

instance FromJSON ExecEnv

-- | config information required
data Configuration = Configuration
    { dbName       :: !String
    , dbUser       :: !String
    , dbPass       :: !String
    , dbHost       :: !String
    , logType      :: ![LogType]
    , logLevel     :: !LogLevel
    , serverPort   :: !Int
    , maxImageSize :: !Double
    , execEnv      :: !ExecEnv
    , authToken    :: !String
    } deriving (Generic,Show)

instance FromJSON Configuration


{-
    get config from a file and if that fails attempt to obtain
    the same from env vars. The json from file or env var is interpreted
    as Configuration type. returns a string upon failure.
-}
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
                Right config    -> return $Right config
                Left errmessage -> return $ Left errmessage


{- read configuration from a file and parse the json to interpret
   it as Configuration type. returns a string error message on failure
   to find the file or failure to parse the contained json.
-}
getConfigFromFile::String->IO (Either String Configuration)
getConfigFromFile  filePath = do
    fileExists <- liftIO $doesFileExist filePath
    if fileExists
        then do
            fileData <- BSL.readFile filePath
            let decoded' = decode fileData :: Maybe Configuration
            case decoded' of
                (Just decoded) -> return $Right decoded
                Nothing        -> return $Left $"Could not parse "++configFile
        else return $Left "Could not load config from file"

{-
    read configuration string from envvar and parse the json string to interpret
    it as Configuration type. returns a string error message on failure to find an
    envvar with given name or failure to parse the contained json.
-}
getConfigFromEnv::String->IO(Either String Configuration)
getConfigFromEnv envVarName = do
    configJsonString' <- lookupEnv envVarName
    case configJsonString' of
        Just configJsonString -> do
            let config' = decode (BSL8.pack configJsonString) :: Maybe Configuration
            case config' of
                Just config -> return $Right config
                Nothing -> return $Left $"Unable to parse config obtained from env var : "++envVarName
        Nothing -> return $ Left $"Could not find env var"++envVarName
