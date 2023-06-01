{-
    Config Module
    exports a getConfigFromEnv function to read from a config.json at root and
    returns a configuration type.
-}

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module EcomApi.Core.Config.Config
    ( getConfigFromEnv
    ) where


import System.Environment
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy.Char8 as BSL8
import EcomApi.Core.Config.Types


-- name of the env var to look for if fetching from file fails
configVarName = "CONFIG"

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
