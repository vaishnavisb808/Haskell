{-|
    Module          : ShopperAPI.Api.Handler.ProductStatusHandler
    Description     : Handler for /updatingproductstatus
-}
{-# LANGUAGE OverloadedStrings #-}

module           ShopperAPI.Api.Handler.ProductUnavailbiltyHandler where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Servant
import           ShopperAPI.Api.Types
import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types



-- | Handler function for updatingproductstatus endpoint.
markProductStatusUnavailable ::Int -> ApiHandler( ApiResponse String)
markProductStatusUnavailable  productid = do
    if productid > 0
        then do
            dbops <- asks envDbOps
            getProductStatus <- liftIO $ getprdStatus dbops productid
            case getProductStatus of
                Right 1-> return $ ApiResponse ("Productid " ++ show productid ++ " is marked as unavailable " ) Nothing 200
                Left msg -> throwError $ jsonError500 "Given productid is not found"
    else throwError $ jsonError500  "invalid productid"
