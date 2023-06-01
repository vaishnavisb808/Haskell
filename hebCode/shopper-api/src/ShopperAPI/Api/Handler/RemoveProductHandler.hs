{-|
    Module          : ShopperAPI.Api.Handler.RemoveProductHandler
    Description     : Handler for /removeproduct
-}

{-# LANGUAGE OverloadedStrings #-}

module            ShopperAPI.Api.Handler.RemoveProductHandler where


import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Servant
import           ShopperAPI.Api.Types
import           ShopperAPI.Core.Config.Types       (Env (envDbOps))
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types
import           ShopperAPI.Services.Logger.Logger
import           ShopperAPI.Services.Logger.Types


-- | Handler function for /removeproduct endpoint
removeProduct :: Int -> ApiHandler( ApiResponse String)
removeProduct productid = withTimeLog RemoveProdHandle $ do
    let logger  = removeProductHandleLog
    if productid>0
        then do
            dbOps <- asks envDbOps
            noOfRowsDeleted <- liftIO $  deleteProductInfo dbOps productid
            case noOfRowsDeleted of
                Right 1 -> do
                    logger INFO "The product has been Removed from the inventory" 200
                    return $ ApiResponse "The product has been Removed \
                                        \ from the inventory" Nothing 200
                Right _ -> do
                    logger ERROR "invalid productid" 500
                    throwError $ jsonError500 "invalid productid"
                Left msg -> do
                    logger ERROR "deletion failed" 500
                    throwError $ jsonError500 "deletion unsuccessful"
    else do
        logger ERROR "provide valid product id" 500
        throwError $ jsonError500 "invalid productid"
