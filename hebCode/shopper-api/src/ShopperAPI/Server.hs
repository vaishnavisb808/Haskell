{-|
    Module :ShopperAPI.Server
    Description : Entrypoint for server
    Functions to start and run server.
-}


{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module           ShopperAPI.Server where


import           Control.Concurrent                                 (forkIO,
                                                                     newChan)
import           Control.Monad.Except                               (MonadIO (liftIO))
import           Control.Monad.IO.Class                             (MonadIO (liftIO),
                                                                     liftIO)
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.Text                                          as T
import           Data.Text.Encoding                                 (encodeUtf8)
import           Data.UUID.V4                                       (nextRandom)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server
import           ShopperAPI.Api.Handler.AddAddressHandlers
import           ShopperAPI.Api.Handler.AddOrRemoveWHManager
import           ShopperAPI.Api.Handler.AddProductHandler
import           ShopperAPI.Api.Handler.AddorRemoveCategoryHandler
import           ShopperAPI.Api.Handler.InsertAndRemoveCartHandler
import           ShopperAPI.Api.Handler.LoginHandler                (loginHandler)
import           ShopperAPI.Api.Handler.MarkAsShipped
import           ShopperAPI.Api.Handler.OrderStatusUpdateHandler
import           ShopperAPI.Api.Handler.ProductUnavailbiltyHandler
import           ShopperAPI.Api.Handler.RemoveProductHandler
import           ShopperAPI.Api.Handler.AddCommentOnaProducthandler(insertComment )
import           ShopperAPI.Api.Handler.TemporaryProductChange
import           ShopperAPI.Api.Handler.UpdateStockHandlers         (updateStock)
import           ShopperAPI.Api.Handler.UserSignupHandler
import           ShopperAPI.Api.Handler.ViewAllProductsHandler      (viewProductsHandler)
import           ShopperAPI.Api.Handler.ViewCartHandler
import           ShopperAPI.Api.Handler.ViewProductsByRatingHandler (getProductsHandler)
import           ShopperAPI.Api.Handler.ViewWishlistHandler         (viewWishlistUser)
import           ShopperAPI.Api.MiddleWare.Auth.Types               (AccessData (..),
                                                                     Role (..))
import           ShopperAPI.Core.Types                              (Tracking (date, orderid, refNo))
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Postgres
import           ShopperAPI.Services.Database.Types

import           ShopperAPI.Api.Handler.EditProductHandler
import           ShopperAPI.Api.Types
import           ShopperAPI.Core.Config.Config                      (getConfig)
import           ShopperAPI.Core.Config.Types                       (Env (Env),
                                                                     authKey,
                                                                     envConfig,
                                                                     envRequestId,
                                                                     serverPort)
import           ShopperAPI.Services.Logger.Logger                 (getLogger,
                                                                    logLoop)
import           ShopperAPI.Services.Logger.Types                  (Log (EventLog),
                                                                    LogLevel (ERROR, INFO),
                                                                    runLog)
import           ShopperAPI.Api.Handler.CartQunatityUpdateHandler
import           System.IO

-- | Load the configuration and start the app
startApp :: IO()
startApp = do
    config <- getConfig
    hSetBuffering stdout NoBuffering
    case config of
        Right config -> do
            let logger =  getLogger config
            connPool <-  initConnectionPool config
            case connPool of
                Right pool -> do
                  runLog logger $ EventLog INFO "Connection to DB" "DB_CONNECT" "success"
                  --  get database functions
                  let postgresDbOps = getDbOps pool
                  initializeSchemaCall  postgresDbOps logger
                  --  new channel acting as queue for logs
                  ch <- newChan
                  --  continuously listen for logs coming over channel
                  forkIO $ logLoop ch logger
                  --create env variable shared across handlers via ReaderT
                  let env = Env config postgresDbOps ch Nothing
                  run portNumber (app env)
                Left err   -> do
                  runLog logger $ EventLog ERROR
                                "failed to connect to DB" "DB_CONNECT" "failure"
            where
                portNumber = serverPort config
        Left msg -> print msg

-- | function for initialize schema logger
initializeSchemaCall  postgresDbOps logger= do
  resultDB <- liftIO $ initializeSchema postgresDbOps
  case resultDB of
    Right msg -> runLog logger $ EventLog INFO
                               "Schema inizialitzation" "Schema_inizialitzation" "success"
    Left err -> do
                  let logMessage = "Schema inizialization failed." ++ show err
                  runLog logger $ EventLog ERROR
                                logMessage "Schema_inizialitzation" "failure"

server :: ServerT API ApiHandler
server = addcategory :<|> removecategory :<|> markAsUnavilable :<|> updStock :<|> userSignup
         :<|> viewproducts :<|> addaddress :<|> login :<|> editProduct :<|> removeproduct
         :<|> viewAllProducts :<|> insWareHouseManager :<|> rmvWarehouseManager :<|> markasshipped
         :<|> viewcart :<|> addProduct :<|> addProductTocart :<|> removeProductFromcart
         :<|> setorderstatus  :<|>temporarypricechange  :<|> addCommentForProduct
         :<|> viewWishlist :<|>cartquantityupdation
  where

    addcategory (Authenticated accessData) categoryName =
              tokenAccess
                  accessData
                  [Manager]
                  (addCategory categoryName )
    addcategory x _        = throwError $ jsonError401 "Invalid credentials"

    removecategory (Authenticated accessData ) catId =
              tokenAccess
                  accessData
                  [Manager]
                  (deleteCategory catId)
    removecategory x _  = throwError $ jsonError401 "Invalid credentials"


    markAsUnavilable (Authenticated accessData) productids =
               tokenAccess
                  accessData
                  [Manager, WareHouseManager]
                  (markProductStatusUnavailable  productids )
    markAsUnavilable x  _ = throwError $ jsonError401 "Invalid Credential"

    updStock (Authenticated accessData)  stockCountlastUpdate =
               tokenAccess
                  accessData
                  [Manager, WareHouseManager]
                  (updateStock stockCountlastUpdate accessData)
    updStock x _    = throwError $jsonError401 "Invalid Credentials"


    userSignup credentials = userSignupHandler credentials
    userSignup x           = throwError err401 {errBody = "Invalid Details"}

    viewproducts (Authenticated accessData) sortby offset =
               tokenAccess
                  accessData
                  [Manager,WareHouseManager]
                  (getProductsHandler sortby offset)
    viewproducts x _  _  = throwError $ jsonError401 "Invalid Credentials"


    addaddress (Authenticated accessData) request=
                tokenAccess
                    accessData
                    [User]
                    (addAddress request accessData)
    addaddress _ _     = throwError $jsonError401 "Invalid Credentials"

    addCommentForProduct (Authenticated accessData) commentproduct=
                tokenAccess
                    accessData
                    [User]
                    (insertComment commentproduct accessData)
    addCommentForProduct _ _     = throwError $jsonError401 "Invalid Credentials"

    login reqBody = loginHandler reqBody
    login _       = throwError err400 {errBody = "Invalid Request"}
-- | check authoeization is correct
    editProduct (Authenticated accessData)  rqst = tokenAccess accessData
                                                                        [Manager]
                                                                        (editProductDetails rqst)
    editProduct _ _ = throwError $ jsonError401 "Invalid Credentials"

    removeproduct (Authenticated accessData) productid =  tokenAccess accessData [Manager] (removeProduct  productid)
    removeproduct _ _  = throwError $ jsonError401 "Invalid Credentials"

    viewAllProducts (Authenticated accessData) category lastId =
               tokenAccess
                   accessData
                   [User]
                   (viewProductsHandler category lastId)
    viewAllProducts x _ _      = throwError err401 {errBody = "Invalid Credentials"}

    insWareHouseManager (Authenticated accessData) cred =
               tokenAccess
                 accessData
                 [Manager]                   (insertWareHouseManager  cred )
    insWareHouseManager _ _=  throwError err401 {errBody = "Invalid creaditals"}

    rmvWarehouseManager (Authenticated accessData) wareHouseManagerid =
               tokenAccess
                 accessData
                 [Manager]                   (removeWareHouseManager wareHouseManagerid )
    rmvWarehouseManager _ _=  throwError err401 {errBody = "Invalid creaditals"}

    markasshipped (Authenticated accessData) request =
                     tokenAccess
                     accessData
                     [WareHouseManager]
                     (insertShippingDetails request)
    markasshipped _ _      = throwError $ jsonError403 "invalid credentials"

    viewcart (Authenticated accessData) =  tokenAccess accessData [User] (viewCartHandler accessData)
    viewcart x  = throwError $ jsonError403 "Invalid Credentials"


    addProduct (Authenticated accessData) request =
                tokenAccess
                    accessData
                    [Manager]
                     (addProducts request accessData)
    addProduct _ _      = throwError $ jsonError401 "Invalid Credentials"

    addProductTocart (Authenticated accessData) proid =
        tokenAccess
           accessData
           [User] (addToCart proid accessData)
    addProductTocart _ _  = throwError $ jsonError401 "invalid credentials"

    removeProductFromcart (Authenticated accessData) proid =
       tokenAccess
         accessData
         [User] (removeFromCart proid)
    removeProductFromcart _ _  = throwError$ jsonError401 "invalid credentials"

    setorderstatus (Authenticated accessData) orderid status=
              tokenAccess
                   accessData
                   [WareHouseManager]
                   (acceptOrRejectOrder orderid status)
    setorderstatus _ _ _= throwError $jsonError401 "Invalid Credentials"

    temporarypricechange (Authenticated accessData) request =
                    tokenAccess
                    accessData
                    [Manager]
                     ( addtTempProductPriceValue request )
    temporarypricechange _ _ = throwError $ jsonError404 "Invalid Credentials"
    
    viewWishlist (Authenticated accessData) = tokenAccess accessData
                                                  [User]
                                                  (viewWishlistUser accessData)
    viewWishlist _                          = throwError
                                               $ jsonError401
                                               "Invalid Credentials"

    cartquantityupdation (Authenticated accessData) productId quantity=
              tokenAccess 
                   accessData 
                   [User]
                   (cartQuantityUpdate productId quantity accessData)
    cartquantityupdation _ _ _ = throwError $ jsonError401 "Invalid Credentials"

-- | function for token access checking
tokenAccess accessData roles action = do
  let user = role accessData
  if user `elem` roles
    then action
    else throwError $ jsonError403 "Access denied"


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
    body = object ["code".= (404 :: Int),"error".= ("Path Not Found"::String),"result".= Null]
