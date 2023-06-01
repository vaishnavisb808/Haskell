{-|
    Module          : ShopperAPI.Api.Handler.OrderStatusUpdateHandler
    Description     : Handler for /orderstatus
-}
{-# LANGUAGE OverloadedStrings     #-}


module              ShopperAPI.Api.Handler.OrderStatusUpdateHandler where

import              Servant
import              Control.Monad.IO.Class
import              Control.Monad.Trans.Reader

import              ShopperAPI.Services.Database.Types
import              ShopperAPI.Core.Config.Types
import ShopperAPI.Api.Types as AT
                                            ( ApiHandler,
                                            ApiResponse(ApiResponse),
                                            AcceptOrReject(Accept, Reject) ) 
import              ShopperAPI.Core.Utils
-- import              ShopperAPI.Core.Types as CT (AcceptOrReject(Accept, Reject) )
import              ShopperAPI.Services.Logger.Logger 
import              ShopperAPI.Core.Transformers   
import              ShopperAPI.Services.Logger.Types     


-- handler function for endpoint orderstatus
acceptOrRejectOrder ::Int -> Maybe AcceptOrReject ->ApiHandler( ApiResponse String)
acceptOrRejectOrder orderId ordeStatus = withTimeLog StatusUpdateHandle $ do
  let logger = orderStatusUpdateHandleLog
  if orderId > 0
      then do  
          dbOps <- asks envDbOps
          case ordeStatus of
              Just x ->do 
                   ordStatus <- liftIO $  orderStatusUpdation dbOps orderId (convertApiOrderStatusToCore x)
                   case ordStatus of
                       Right 1 ->do
                           if ordeStatus == Just Accept
                              then do
                                  let logMessage= "order accepted "
                                  logger INFO logMessage 200
                                  return $ ApiResponse "Order has been accepted " Nothing 200
                              else do
                                  let logMessage= "Order rejected "
                                  logger INFO logMessage 200
                                  return $ ApiResponse "Order has been rejected and refund is initiated" Nothing 200
                       Left err -> do
                           logger ERROR (show err) 500
                           throwError $ jsonError500 "order status updation unsuccessful"
              Nothing -> throwError $ jsonError500 "invalid status or status not found"
      else throwError $ jsonError500 "invalid orderid"