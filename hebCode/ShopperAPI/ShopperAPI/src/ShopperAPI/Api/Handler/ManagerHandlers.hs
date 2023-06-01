{-# LANGUAGE OverloadedStrings     #-}


module              ShopperAPI.Api.Handler.ManagerHandlers where

import              Servant
import              Control.Monad.IO.Class
import              ShopperAPI.Services.Database.Types
import              Control.Monad.Trans.Reader
import              ShopperAPI.Core.Config.Types
import              ShopperAPI.Api.Types

-- removeProduct :: Int ->ApiHandler( ApiResponse String)
-- removeProduct productid = do
--     if productid>0 
--         then do
--             dbOps <- asks envDbOps
--             dbresult <- liftIO $  deleteInfo dbOps productid 
--             case dbresult of
--                 Right 1 -> return $ ApiResponse "deletion success" Nothing 200
--                 Left msg -> throwError err500 {errBody = "deletion unsuccessful"}
--     else throwError err500 {errBody = "invalid productid"}

acceptOrRejectOrder ::Int ->OrderAccept  ->ApiHandler( ApiResponse String)
acceptOrRejectOrder orderId orderStatus = do
  if orderId > 0
      then do
          dbOps <- asks envDbOps
          let order_status = status orderStatus
          orderStatus <- liftIO $  orderConfirmation dbOps orderId order_status
          case orderStatus of
                Right 1  -> return $ ApiResponse "order status updated " Nothing 200
                Left msg -> throwError err500 {errBody = "order status updation unsuccessful"}
    else throwError err500 {errBody = "invalid orderid"}

