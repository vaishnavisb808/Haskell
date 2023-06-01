{-|
    Module : EcomApi.Api.Handler.ModifyLogic
    Description : Handler functions for /modifylogic
    Handler function for /modify logic
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module EcomApi.Api.Handler.ModifyLogic(updateLogicHandler) where

import           Control.Monad                      (mplus)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Trans.Reader         (asks)
import           Data.Maybe                         (fromMaybe)
import           Data.Text.Internal.Encoding.Utf32  (validate)
import           Data.Time                          (Day (ModifiedJulianDay),
                                                     UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple.ToField (ToField (toField))
import           EcomApi.Api.Middleware.Auth.Types  (AppData)
import qualified EcomApi.Api.Types                  as AT (ApiHandler,
                                                           ApiResponse (ApiResponse),
                                                           Logic (..),
                                                           Request (Request),
                                                           Rule,
                                                           StoreException (StoreException),
                                                           ZoneException (ZoneException),
                                                           effective,
                                                           markupBasisPoints)
import           EcomApi.Core.Config.Types          (Env (envDbOps),
                                                     envRequestId)
import           EcomApi.Core.Transformers          (convertApiTypeToCoreType)
import           EcomApi.Core.Types                 as CT (Logic (..))
import           EcomApi.Core.Utils                 (jsonError400, jsonError500)
import           EcomApi.Services.Database.Types    (DbError, DbOps (..))
import           EcomApi.Services.Logger.Logger     (logToChannel,
                                                     modifyLogicLog,
                                                     withTimeLog)
import           EcomApi.Services.Logger.Types      (Endpoint (ModifyLogic),
                                                     HttpMethod (GET), Log (..),
                                                     LogLevel (ERROR, INFO))
import           GHC.Int                            (Int64)
import           Servant                            (throwError)
import           Servant.Server                     (err400, err500, errBody)


type Logger = LogLevel -> String -> Integer -> AT.ApiHandler ()


{- | Handler function for /modifyLogic endpoint.
        1. Validate request
        2. covert API Type to core type
        3. Insert new or update database based on whether a future logic exists for UPC
-}
updateLogicHandler :: AppData -> AT.Request -> AT.ApiHandler (AT.ApiResponse String)
updateLogicHandler user request = withTimeLog user ModifyLogic $
    handleRequest . convertApiTypeToCoreType =<< validateRequest request logger
  where
    logger = modifyLogicLog user

    handleRequest :: CT.Logic -> AT.ApiHandler (AT.ApiResponse String)
    handleRequest logic = do
        dbOps <- asks envDbOps
        -- ^ lookup upc table to get info of the latest logic for given upc
        dbResponse <- liftIO (getupcData dbOps (CT.upc logic))
        presentTime <- liftIO  getCurrentTime
        handleDbResponse dbResponse logic presentTime dbOps

    -- ^ insert or modify based on whether a future logic exists for upc
    handleDbResponse :: Either DbError [(UTCTime,Int)]
                     -> CT.Logic
                     -> UTCTime
                     -> DbOps
                     -> AT.ApiHandler (AT.ApiResponse String)
    handleDbResponse upcData logic presentTime dbOps =
        case upcData of
            Right []                 -> insertNewLogic logic dbOps [] logger
            Right [(effFrm,logicid)] -> if effFrm > presentTime
                                            then modifyExistingLogic logic
                                                                     logicid
                                                                     dbOps
                                                                     logger
                                            else insertNewLogic logic
                                                                dbOps
                                                                [(effFrm,logicid)]
                                                                logger
            Right dataList           -> do
                logger ERROR ("corrupt upc data"++show dataList) 500
                throwError $ jsonError500 "Updation failed, try again."
            Left err                 -> do
                logger  ERROR (show err) 500
                throwError $ jsonError500 "Updation failed, try again."


-- | insert new logic to db
insertNewLogic :: CT.Logic
               ->  DbOps
               -> [(UTCTime,Int)]
               -> Logger
               -> AT.ApiHandler (AT.ApiResponse String)
insertNewLogic logic dbOps upcData logger =do
    liftIO (insLogic dbOps logic upcData) >>= handleDbResponse
  where
    handleDbResponse :: Either DbError Int64 -> AT.ApiHandler (AT.ApiResponse String)
    handleDbResponse insertStatus = case insertStatus of
        Left err -> do
                    logger ERROR (show err) 500
                    throwError $ jsonError500 "Updation failed, try again."
        Right y -> do
                   let logMessage= "Inserted logic for upc " ++ show (upc logic)
                   logger  INFO logMessage 200
                   return $ AT.ApiResponse "Inserted logic" Nothing 200


-- | modify an existing logic
modifyExistingLogic :: CT.Logic
                    -> Int
                    -> DbOps
                    -> Logger
                    -> AT.ApiHandler (AT.ApiResponse String)
modifyExistingLogic logic logicId dbOps logger =
    liftIO (modLogic dbOps logicId logic) >>= handleDbResponse
  where
      handleDbResponse :: Either DbError Int64 -> AT.ApiHandler (AT.ApiResponse String)
      handleDbResponse rowsUpdated = case rowsUpdated of
            Left err -> do
                        logger ERROR (show err) 500
                        throwError $ jsonError500 "Updation failed, try again."
            Right y  -> do
                        let message = "Updated logic for upc " ++ show (upc logic)
                        logger INFO message 200
                        return $ AT.ApiResponse message Nothing 200


-- |validate request body for modify logic
validateRequest :: AT.Request -> Logger -> AT.ApiHandler AT.Request
validateRequest (AT.Request logic upc) logger = do
    let effectiveDate = AT.effective logic
    presentTime <- liftIO  getCurrentTime
    let  storeException' = fromMaybe [] (AT.storeExceptions logic)
    let  zoneException = fromMaybe [] (AT.zoneExceptions  logic)
    -- check if effective date is a future date, only future logic can be modified
    if effectiveDate > presentTime
       then do
           let validationResult =  validateUpc upc `mplus`
                                   validateGeneralMarkup (AT.rule logic) `mplus`
                                   validateStoreExceptions storeException' `mplus`
                                   validateZoneExceptions zoneException `mplus`
                                   validateStoreMarkup storeException' `mplus`
                                   validateZoneMarkup zoneException
           case validationResult of
              Just err -> do
                logger INFO (show err ) 400
                throwError $ jsonError400 err
              Nothing  -> return $ AT.Request logic upc
        else do
            presentTime <- liftIO  getCurrentTime
            logger INFO ("Invalid date."++show(AT.effective logic)++" current time :"++show presentTime) 400
            throwError $ jsonError400 "Invalid date. Only future logic can be entered!"


-- | Upc validation => positive number with less than 12 digits
validateUpc :: Integer -> Maybe String
validateUpc upc = if upc >= 0 && upc < 1000000000000
                     then Nothing
                     else Just ("Invalid UPC . It should be a positive Integer with a\
                                                \ max limit of 12 digits " ++ show upc)


-- | GeneralMarkup validation => postive number with max 4 digits
validateGeneralMarkup :: AT.Rule -> Maybe String
validateGeneralMarkup rule = if AT.markupBasisPoints rule >= 0 &&
                                AT.markupBasisPoints rule < 10000
                                then Nothing
                                else Just ("Invalid markupBasisPoint" ++ show rule)


-- | storeException validation => store: postive number with max 5 digits
--   zone: positive number with max 5 digits
validateStoreExceptions :: [AT.StoreException] -> Maybe String
validateStoreExceptions strExcps = validateList strExcps
                                                condition
                                                "Invalid store or zone number "
  where
    condition :: AT.StoreException -> Bool
    condition (AT.StoreException rule store zone) = ( store > 0 && store < 100000 ) &&
                                                    (zone > 0 && zone < 100000 )


-- | zoneException validation => positive number with max 5 digits
validateZoneExceptions :: [AT.ZoneException] -> Maybe String
validateZoneExceptions znExcps = validateList znExcps condition "Invalid zone number "
  where
    condition :: AT.ZoneException -> Bool
    condition = \(AT.ZoneException rule zone) -> zone > 0 && zone < 100000


-- | Function for zoneMarkup validation
validateZoneMarkup :: [AT.ZoneException] -> Maybe String
validateZoneMarkup znExcps = validateList znExcps condition "Invalid zone markupBasisPoint"
  where
    condition :: AT.ZoneException -> Bool
    condition = \(AT.ZoneException rule zone) ->
        AT.markupBasisPoints rule> 0 && AT.markupBasisPoints rule< 10000


-- | storeMarkup validation => positive number with max 4 digits
validateStoreMarkup :: [AT.StoreException] -> Maybe String
validateStoreMarkup strExcps = validateList strExcps
                                                condition
                                                "Invalid store or zone markupBasisPoint "
  where
    condition :: AT.StoreException -> Bool
    condition =
        \(AT.StoreException rule store zone) ->
            AT.markupBasisPoints rule  > 0 && AT.markupBasisPoints rule < 10000



{-| validate list of values against a condition , return Nothing if condition is true
    for all elements and Just errMessage when atleast one of them fails , where errMsg is
    the message appended with the string representation of list of failed elements
-}
validateList :: ( Show a ) => [a] -> ( a -> Bool ) -> String -> Maybe String
validateList [] _ _ = Nothing
validateList list condition msg=
    case  filter (not.condition) list  of
        []            -> Nothing
        listOfInvalid -> Just $ msg ++ show listOfInvalid
