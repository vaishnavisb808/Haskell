{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module             ShopperApi.Server where

import              ShopperAPI.Core.Config.Config
import              ShopperAPI.Core.Config.Types
import              ShopperAPI.Services.Database.Postgres
import              ShopperAPI.Services.Database.Types
import              ShopperAPI.Api.Types
import              Servant
import              Servant.Auth.Server
import              Control.Monad.Trans.Reader
import              Network.Wai.Handler.Warp
import              ShopperAPI.Api.Handler.LoginHandler
import              Data.Aeson
import qualified    Data.Text                         as T
import              Data.Text.Encoding                (encodeUtf8)
import              ShopperAPI.Api.Handler.ManagerHandlers




startApp :: IO()
startApp = do
    config <- getConfig
    case config of
        Right config -> do
            connPool <-  initConnectionPool config
            case connPool of
                Right pool -> do
                    --  get database functions
                    let postgresDbOps = getDbOps pool
                    initializeSchema postgresDbOps
                    print "connection to db established"
                    let env = Env config postgresDbOps
                    run portNumber (app env)
                Left err -> print "connection to db failed"
            where
                portNumber = serverPort config
        Left msg -> print msg

server :: ServerT API ApiHandler
server = orderacceptance
  where
      
       orderacceptance (Authenticated mgr)orderid status = acceptOrRejectOrder orderid status 
       orderacceptance _ _      = throwError err401 {errBody = "Invalid Credentials"}
    -- login reqbody    = loginfunction reqbody
    -- login _          = throwError err401 {errBody = "Invalid Credentials"}

    -- removeproduct (Authenticated mgr) productid = removeProduct productid
    -- removeproduct _ _      = throwError err401 {errBody = "Invalid Credentials"}


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
            (Proxy :: Proxy '[CookieSettings, JWTSettings, BasicAuthCheck AccessData])
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
