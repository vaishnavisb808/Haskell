{-|
    Module          : ShopperAPI.Api.Handler.ViewAllProductsHandler
    Description     : Handler for /viewing products by the user
-}

{-# LANGUAGE OverloadedStrings #-}


module           ShopperAPI.Api.Handler.ViewAllProductsHandler where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Reader         (asks)
import           Data.Maybe
import           Servant                            (Handler, throwError)
import           ShopperAPI.Api.Types               (ApiHandler,
                                                     ApiResponse (ApiResponse),
                                                     ProductResponse)
import           ShopperAPI.Core.Config.Types       (Env, envDbOps)
import           ShopperAPI.Core.Utils              (jsonError400, jsonError404,
                                                     jsonError500)
import           ShopperAPI.Services.Database.Types (DbOps (DbOps),
                                                     viewProductsByUser)
import           ShopperAPI.Core.Transformers (convertToProductResponse )                              

-- | Handler function for viewing products by user endpoint.
viewProductsHandler ::Maybe String -> Maybe Int -> ApiHandler( ApiResponse ProductResponse)
viewProductsHandler category lastId = do
    dbOps <- asks envDbOps
    let pageKey = fromMaybe 0 lastId
    if pageKey<0
        then throwError $jsonError400 $ "Invalid product id " ++ show pageKey
        else do
             dbresult' <- liftIO $ viewProductsByUser dbOps category lastId
             case dbresult' of
                 Left e -> throwError $ jsonError500 "sql error"
                 Right ([],Nothing) ->  throwError $ jsonError400 "No products are available"
                 Right (productList,nextPageKey) -> do
                     let dbresult = convertToProductResponse productList nextPageKey
                     return $ ApiResponse dbresult Nothing 200
