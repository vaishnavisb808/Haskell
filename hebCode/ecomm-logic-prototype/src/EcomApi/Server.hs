{-|
    Module : EcomApi.Server
    Description : Entrypoint for server
    Functions to start and run server.
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module EcomApi.Server where

import           Control.Concurrent
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Char8              as C
import           Data.ByteString.Lazy.Char8         as BSL8
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (encodeUtf8)
import           EcomApi.API.Handler.ModifyLogic    (updateLogicHandler)
import           EcomApi.API.Handler.ViewLogic      (getLogicEndPoint)
import           EcomApi.API.Types
import           EcomApi.Core.Utils
import           EcomApi.Core.Config.Config
import           EcomApi.Core.Config.Types
import           EcomApi.Core.Transformers          (convertApiTypeToCoreType)
import           EcomApi.Services.Database.Postgres
import           EcomApi.Services.Logger.Logger
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server
import           System.Environment                 as ENV

--server :: ServerT API ApiHandler
server = getLogic :<|> postLogic
  where
    getLogic (Authenticated usr) id = getLogicEndPoint id
    getLogic x id = throwError $jsonError401 "Unauthorized" 

    postLogic (Authenticated usr) request = updateLogicHandler request 
    postLogic x msg = throwError $jsonError401  "Unauthorized" 



-- | load all configuration and start the server
startApp :: IO ()
startApp = do
    config <- getConfig
    case config of
        Right config -> do
           let logger =  getLogger config  -- get logger with config
           logInfo logger "Connecting to DB ..."
           --  create a connection pool
           connPool <- initConnectionPool config
           logInfo logger ("Server running on Port" ++ show portNumber)
           --  get database functions
           let postgresDbOps = getDbOps connPool
           --  new channel acting as queue for logs
           ch <- newChan
           --  continuously listen for logs coming over channel
           forkIO $ logLoop ch logger
           let env = Env config postgresDbOps ch
           run portNumber (app env)
          where
            portNumber = serverPort config
        Left errMsg -> print errMsg

-- | natural transformation to convert custom monad back to Handler
nt :: Env -> ApiHandler a -> Handler a
nt env apiHandler = runReaderT apiHandler env


app :: Env -> Application
app env = do
    let key = fromSecret . encodeUtf8 . T.pack . authKey . envConfig $ env
    let jwtCfg = defaultJWTSettings key
    serveWithContext
        api
        (customFormatters Servant.:.jwtCfg :. defaultCookieSettings :. EmptyContext)
        (hoistServerWithContext
            api
            (Proxy :: Proxy '[CookieSettings, JWTSettings])
            (nt env)
            server
        )

api :: Proxy API
api = Proxy

-- | Custom formatters used to give json formatted error messages
customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = customFormatter
  , urlParseErrorFormatter   = customFormatter
  , notFoundErrorFormatter   = notFoundFormatter
  }

-- | customFormatter for formatting error messages in json format
customFormatter :: ErrorFormatter
customFormatter combntr req err = err400
    { errBody = encode body
    , errHeaders = [("Content-Type", "application/json")]
    }
  where
    body = object ["code".=(400::Int),"error".=err,"result".=Null]


-- | Custom formatter for not found error
notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req = err404
    { errBody = encode body
    , errHeaders = [("Content-Type", "application/json")]
    }
  where
    body = object ["code".=(404::Int),"error".=("Path Not Found"::String),"result".=Null]
