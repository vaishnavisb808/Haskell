{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module           ShopperAPI.Api.Handler.MarkAsShipped where

import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Trans.Reader         (asks)
import           Data.Time                          (UTCTime, getCurrentTime)
import           GHC.Int                            (Int64)
import           Servant
import           ShopperAPI.Api.Types               as AT 
import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Core.Types              as CT
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types
import           ShopperAPI.Services.Logger.Logger  (markAsShippedHandleLog,
                                                     withTimeLog)
import           ShopperAPI.Services.Logger.Types   (EndPoint (MarkAsShippedHandle),
                                                     LogLevel (ERROR, INFO))
import ShopperAPI.Core.Transformers ( convertTrackingApiToCore )



-- | Handler function for markAsShipped endpoint
insertShippingDetails :: AT.Tracking-> ApiHandler ( ApiResponse String)
insertShippingDetails request = withTimeLog MarkAsShippedHandle $ do
    let logger = markAsShippedHandleLog
        converted = convertTrackingApiToCore request
    currentTime <- liftIO getCurrentTime
    validatedRequest <- validateShippingDetails converted currentTime
    dbOps <- asks envDbOps
    dbresult <- liftIO $ shipInfo dbOps converted
    returnDbResult dbresult

returnDbResult :: Either DbError Int64 -> ApiHandler (ApiResponse String)
returnDbResult dbresult  = do
   let logger = markAsShippedHandleLog
   case dbresult of
            Right 1 -> do
                       logger INFO "Shipping details added successfully" 200
                       return $ ApiResponse "Shipping details added successfully" Nothing 200
            Right _ -> do
                       logger ERROR "invalid" 500
                       throwError $ jsonError500 "invalid"
            Left msg -> do
                       logger ERROR "Addition unsuccessful" 500
                       throwError $ jsonError500 "Addition unsuccessful"


validateShippingDetails :: CT.Tracking -> UTCTime -> ApiHandler CT.Tracking
validateShippingDetails request currentTime = do
  let logger = markAsShippedHandleLog
  if  null (CT.refNo request)
      then do
          logger ERROR "Reference no is missing, not able to update the status" 500
          throwError $ jsonError500 "Reference no is missing, not able to update the status"
      else
          if CT.orderid request > 0
              then
                  if CT.date request < currentTime
                      then return request
                      else do
                          logger ERROR "invalid time" 500
                          throwError $ jsonError500 "invalid time"
              else do
                  logger ERROR "invalid orderid" 500
                  throwError $ jsonError500 "invalid orderid"
