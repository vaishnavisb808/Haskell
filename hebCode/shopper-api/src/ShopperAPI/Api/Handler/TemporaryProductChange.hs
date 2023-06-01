{-|
    Module : ShopperApi.Api.Handler.ManagerHandlers
    Description : Handler functions for /ManagerHandlers
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}



module           ShopperAPI.Api.Handler.TemporaryProductChange where

import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Reader           (asks)
import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (Object), object,
                                                       parseJSON, toJSON, (.:),
                                                       (.=))
import           Data.ByteString.Lazy                 hiding (pack, unpack)
import           Data.Text                            (unpack)
import           Data.Time                            (Day, UTCTime (UTCTime),
                                                       secondsToDiffTime,
                                                       utctDay)
import           GHC.Generics                         (Generic)
import           Servant

import           Servant.Auth.Server                  (Auth, FromJWT, JWT,
                                                       ToJWT)

import           Control.Monad.Except                 (mplus)
import           ShopperAPI.Api.MiddleWare.Auth.Types as MT
import           ShopperAPI.Api.Types                 as AT (AddTempValueInfo (..),
                                                             ApiHandler,
                                                             ApiResponse (ApiResponse))
import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Core.Types                as CT
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types   as DT
import           Text.Read                            (readMaybe)
import           ShopperAPI.Core.Transformers (convertTempAPiToCore)

import           Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Int (Int64)



{- | Handler function for /temporarypricechange endpoint. -}
addtTempProductPriceValue :: AddTempValueInfo -> ApiHandler (ApiResponse String )
addtTempProductPriceValue requests = do
    handleRequestOfTemp .convertTempAPiToCore =<< validateTempRequest requests
  where
    handleRequestOfTemp :: AddTempValueInfo' -> ApiHandler (ApiResponse String)
    handleRequestOfTemp request'  = do
        let addProductId' = CT.addProductId request'
        if addProductId'> 0
            then do
                dbOps <- asks envDbOps
                checkingTcpValue <- liftIO $ checkingproductprice dbOps  request'
                case checkingTcpValue of
                    Right 0 -> do
                        addTcpValue <- liftIO $ addproductPrice  dbOps  request'
                        case addTcpValue of
                            Right addingTcp -> return $ ApiResponse "Adding TCP is success" Nothing 200
                            Left msg -> throwError $ jsonError500 "Something went wrong try again"
                    Right 1  -> throwError $ jsonError400  "Temporary Price Change already exists in the specified time frame"
                    Left msg -> throwError $ jsonError500 "Something went wrong try again"
            else throwError $ jsonError400  "invalid productid"


-- | Function for validating price
validateTempPrice :: Float -> Maybe String
validateTempPrice tempPriceForProduct =
    if  tempPriceForProduct >= 0
        then Nothing
        else Just "Price change not possible. Product value too low"

-- | Function for validating currency
validateTempCurrency :: String  ->  Maybe String
validateTempCurrency tempCurrencycheck =
    case tempCurrencycheck of
        "$" -> Nothing
        ""  -> Just "Please fill all the fields"
        _   -> Just "Only $ can be accepted as currency"
-- | Function for validating date
validateEndDate ::UTCTime -> UTCTime  ->Maybe String
validateEndDate validateEndDate validateStartDate  = do
    let startDate = utcToInteger validateStartDate
        endDate   = utcToInteger validateEndDate
    if endDate >= startDate
        then Nothing
        else Just "Invalid data type"

utcToInteger:: UTCTime -> Int64
utcToInteger = floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds


-- | validate request body for temppricechange endpoint
validateTempRequest :: AddTempValueInfo  -> AT.ApiHandler AddTempValueInfo
validateTempRequest addTempValues  = do
    let tempValidationResult    = validateTempPrice (AT.addTempValue  addTempValues) `mplus`
                                  validateTempCurrency (AT.addTempCurrecy addTempValues)`mplus`
                                  validateEndDate (AT.addEffectiveStartDate addTempValues) (AT.addEffectiveEndDate addTempValues)
    case tempValidationResult of
        Just err -> do
          throwError $ jsonError400 err
        Nothing -> return addTempValues


  