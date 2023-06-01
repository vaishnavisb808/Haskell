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

import           Control.Concurrent                 (forkIO, newChan)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Reader         (runReaderT)
import           Data.Aeson                         (Value (Null), encode,
                                                     object, (.=))
import qualified Data.ByteString.Char8              as C
import           Data.ByteString.Lazy.Char8         as BSL8
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (encodeUtf8)
import           Data.UUID.V4                       (nextRandom)
import           EcomApi.Api.Handler.Auth           (getToken)
import           EcomApi.Api.Handler.ModifyLogic    (updateLogicHandler)
import           EcomApi.Api.Handler.ViewLogic      (getLogicHandler)
import           EcomApi.Api.Middleware.Auth.Auth   (authcheck)
import           EcomApi.Api.Middleware.Auth.Types  (AppData)
import           EcomApi.Api.Types                  (API, ApiHandler, UPC (UPC))
import           EcomApi.Core.Config.Config         (getConfig)
import           EcomApi.Core.Config.Types          (Env (Env), authKey,
                                                     envConfig, envDbOps,
                                                     envRequestId, serverPort)
import           EcomApi.Core.Transformers          (convertApiTypeToCoreType)
import           EcomApi.Core.Utils                 (jsonError401)
import           EcomApi.Services.Database.Postgres (getDbOps,
                                                     initConnectionPool)
import           EcomApi.Services.Database.Types    (initializeSchema)
import           EcomApi.Services.Logger.Logger     (getLogger, logLoop)
import           EcomApi.Services.Logger.Types      (Log (EventLog),
                                                     LogLevel (ERROR, INFO),
                                                     runLog)
import           Network.Wai                        (Application)
import           Network.Wai.Handler.Warp           (run)
import           Servant                            (BasicAuthCheck,
                                                     Context (EmptyContext, (:.)),
                                                     ErrorFormatter,
                                                     ErrorFormatters, Handler,
                                                     NotFoundErrorFormatter,
                                                     Proxy (Proxy), ServerT,
                                                     bodyParserErrorFormatter,
                                                     defaultErrorFormatters,
                                                     err400, err404, errBody,
                                                     errHeaders,
                                                     hoistServerWithContext,
                                                     notFoundErrorFormatter,
                                                     serveWithContext,
                                                     throwError,
                                                     urlParseErrorFormatter,
                                                     (:<|>) ((:<|>)))
import           Servant.Auth.Server                (Auth,
                                                     AuthResult (Authenticated),
                                                     CookieSettings,
                                                     JWTSettings,
                                                     defaultCookieSettings,
                                                     defaultJWTSettings,
                                                     fromSecret)
import           System.IO                          (BufferMode (NoBuffering),
                                                     hSetBuffering, stdout)


-- | load all configuration and start the server
startApp :: IO ()
startApp = do
    -- get config from environment
    config <- getConfig
    hSetBuffering stdout NoBuffering
    case config of
        Right config -> do
           -- get logger with config
           let logger =  getLogger config
           --  create a connection pool
           connPool <-  initConnectionPool config
           case connPool of
             Right pool -> do
                   runLog logger $ EventLog INFO "Connection to DB" "DB_CONNECT" "success"
                   --  get database functions
                   let postgresDbOps = getDbOps pool
                   initializeSchema postgresDbOps
                   --  new channel acting as queue for logs
                   ch <- newChan
                   --  continuously listen for logs coming over channel
                   forkIO $ logLoop ch logger
                   --create env variable shared across handlers via ReaderT
                   let env = Env config postgresDbOps ch Nothing
                   run portNumber (app env)
             Left err -> do
                   runLog logger $ EventLog ERROR "failed to connect to DB" "DB_CONNECT" "failure"
          where
            portNumber = serverPort config
        Left errMsg -> print errMsg


server :: ServerT API ApiHandler
server = getLogic :<|> postLogic :<|> getToken
  where
    getLogic (Authenticated usr) (UPC upc) lastid = getLogicHandler usr upc lastid
    getLogic _ _ _               = throwError $jsonError401 "Unauthorized"

    postLogic (Authenticated usr) request = updateLogicHandler usr request
    postLogic x msg = throwError $jsonError401  "Unauthorized"


-- | natural transformation to convert custom monad back to Handler
nt :: Env -> ApiHandler a -> Handler a
nt env apiHandler = do
    reqId <- liftIO  nextRandom
    runReaderT apiHandler env{envRequestId=Just reqId}


app :: Env -> Application
app env = do
    let dbOps = envDbOps env
    -- extract jwt auth key from env
    let key = fromSecret . encodeUtf8 . T.pack . authKey . envConfig $ env
    let jwtCfg = defaultJWTSettings key
    serveWithContext
        api
        (customFormatters Servant.:.jwtCfg :. authcheck dbOps :. defaultCookieSettings :. EmptyContext)
        (hoistServerWithContext
            api
            (Proxy :: Proxy '[CookieSettings, JWTSettings,BasicAuthCheck AppData])
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
