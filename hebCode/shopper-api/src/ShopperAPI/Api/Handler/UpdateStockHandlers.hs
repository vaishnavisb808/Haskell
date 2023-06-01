{-|
    Module          : ShopperAPI.Api.Handler.UpdateStockHandlers
    Description     : Handler for /updatingstock
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module           ShopperAPI.Api.Handler.UpdateStockHandlers where

import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Reader           (asks)
import           Servant                              (throwError)
import           ShopperAPI.Api.MiddleWare.Auth.Types as MT (AccessData (id))
import           ShopperAPI.Api.Types                 as AT 
import           ShopperAPI.Core.Types                as CT  (ProductStock(productId,
                                                                           stockCount))
import           ShopperAPI.Core.Config.Types         (Env (envDbOps))
import           ShopperAPI.Core.Utils                (jsonError400,
                                                       jsonError500)
import           ShopperAPI.Core.Transformers         (convertProductStockApiToCore)
import           ShopperAPI.Services.Database.Types   (DbOps (updateStk))



-- | Handler function for updatingproductstock endpoint.

updateStock :: AT.ProductStock -> AccessData -> ApiHandler( ApiResponse String)
updateStock stockUpdateRequest accessData= do
            dbops <- asks envDbOps
            let convertStockUpdateRequest = convertProductStockApiToCore stockUpdateRequest
                prdtId= CT.productId convertStockUpdateRequest
                stockCount' = CT.stockCount convertStockUpdateRequest
                loginid =MT.id accessData
            if prdtId > 0 && stockCount' >= 0 then do
              updateStock <- liftIO $ updateStk dbops prdtId stockCount' loginid
              case updateStock of
                  Right 1->do
                      let  msg = "Stock has been updated to "++ show stockCount' ++ " for productid "++ show prdtId
                      return $ ApiResponse msg Nothing 200
                  --Right _ case need not be considered because product id is unique key and duplicate records are not allowed
                  Right 0 -> throwError $jsonError400 "Productid not found"
                  Left msg -> throwError $jsonError500 "updation unsuccessful"
            else
                throwError $jsonError400 "Invalid productid or stock count"