{-|
    Module : EcomApi.Api.Handler.ModifyLogic
    Description : Handler functions for /modifylogic
    Handler function for /modify logic
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module EcomApi.Api.Handler.ModifyLogic where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Maybe                         (fromMaybe)
import           Data.Text.Internal.Encoding.Utf32  (validate)
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField (ToField (toField))
import qualified EcomApi.Api.Types                  as AT (ApiHandler,
                                                           ApiResponse (ApiResponse),
                                                           Logic (..),
                                                           Request (Request),
                                                           StoreException (StoreException),
                                                           ZoneException (ZoneException),
                                                           Rule,markupBasisPoints,
                                                           effective)
import           EcomApi.Core.Config.Types          (Env (envDbOps))
import           EcomApi.Core.Transformers          (convertApiTypeToCoreType)
import           EcomApi.Core.Types                 as CT (Logic (..))
import           EcomApi.Core.Utils                 (jsonError500,jsonError400
                                                    ,logger,withTimeLog)
import           EcomApi.Services.Database.Types    (DbError, DbOps (..))
import           GHC.Int                            (Int64)
import           Servant                            (throwError)
import           Servant.Server                     (err400, err500, errBody)
import           EcomApi.Services.Logger.Types


-- | Handler function for modifyLogic endpoint.
updateLogicHandler :: AT.Request -> AT.ApiHandler (AT.ApiResponse String)
updateLogicHandler request = withTimeLog $ 
    handleRequest . convertApiTypeToCoreType =<< validateRequest request
  where

    handleRequest :: CT.Logic -> AT.ApiHandler (AT.ApiResponse String)
    handleRequest logic = do
        dbOps <- asks envDbOps
        dbResponse <- liftIO (getupcData dbOps (CT.upc logic))
        presentTime <- liftIO  getCurrentTime
        handleDbResponse dbResponse logic presentTime dbOps

    -- ^ decide whether to insert or modify based on whether a future logic exists for upc
    handleDbResponse :: Either DbError [(UTCTime,Int)]
                     -> CT.Logic
                     -> UTCTime
                     -> DbOps
                     -> AT.ApiHandler (AT.ApiResponse String)
    handleDbResponse upcData logic presentTime dbOps =
        case upcData of
            Right []                 -> insertNewLogic logic True dbOps
            Right [(effFrm,logicid)] -> if effFrm > presentTime
                                            then modifyExistingLogic logic logicid dbOps
                                            else insertNewLogic logic False dbOps
            Right dataList           -> do
                                        logger ERROR (show dataList)
                                        throwError $ jsonError500 "Updation failed, try again."
            Left err                 -> do
                                        logger ERROR (show err)
                                        throwError $ jsonError500 "Updation failed, try again."

-- | insert new logic to db
insertNewLogic :: CT.Logic -> Bool ->  DbOps -> AT.ApiHandler (AT.ApiResponse String)
insertNewLogic logic shouldUpdateUPC dbOps  =
    liftIO (insLogic dbOps logic shouldUpdateUPC) >>= handleDbResponse
  where
    handleDbResponse :: Either DbError Int64 -> AT.ApiHandler (AT.ApiResponse String)
    handleDbResponse insertStatus = case insertStatus of
        Left err -> do
                    logger ERROR (show err)
                    throwError $ jsonError500 "Updation failed, try again."
        Right y -> do
                   let logMessage= "Inserted logic for upc " ++ show (upc logic)
                   logger INFO logMessage
                   return $ AT.ApiResponse "Inserted logic" Nothing 200


-- | modify an existing logic
modifyExistingLogic :: CT.Logic -> Int -> DbOps -> AT.ApiHandler (AT.ApiResponse String)
modifyExistingLogic logic logicId dbOps =
    liftIO (modLogic dbOps logicId logic) >>= handleDbResponse
  where
      handleDbResponse :: Either DbError Int64 -> AT.ApiHandler (AT.ApiResponse String)
      handleDbResponse rowsUpdated = case rowsUpdated of
            Left err -> do
                        logger ERROR (show err)
                        throwError $ jsonError500 "Updation failed, try again."
            Right y  -> do
                        let logMessage= "Updated logic for upc " ++ show (upc logic)
                        logger INFO logMessage
                        return $ AT.ApiResponse "Updated" Nothing 200



-- |validate request body for modify logic
validateRequest :: AT.Request -> AT.ApiHandler AT.Request
validateRequest (AT.Request logic upc) = do
    let effectiveDate = AT.effective logic
    presentTime <- liftIO  getCurrentTime
    let  storeException' = fromMaybe [] (AT.storeExceptions logic)
    let  zoneException = fromMaybe [] (AT.zoneExceptions  logic)
    -- check if effective date is a future date, only future logic can be modified
    if effectiveDate > presentTime
        then case ( validateUpc upc
                  , validateGeneralMarkup (AT.rule logic)
                  , validateStoreExceptions storeException'
                  , validateZoneExceptions zoneException
                  , validateStoreMarkup storeException'
                  , validateZoneMarkup zoneException
                  ) of
                  ( Just err , _ , _ , _ ,_ ,_)      -> do
                                                        logger INFO (show err )
                                                        throwError $ jsonError400 err
                  ( _ , Just err, _ , _ ,_ ,_ )      -> do
                                                        logger INFO (show err )
                                                        throwError $ jsonError400 err
                  ( _ , _ , Just err , _, _,_)       -> do
                                                        logger INFO (show err )
                                                        throwError $ jsonError400 err
                  ( _ , _ , _, Just err, _,_)        -> do
                                                        logger INFO (show err )
                                                        throwError $ jsonError400 err
                  ( _ , _ , _, _,Just err,_)         -> do
                                                        logger INFO (show err )
                                                        throwError $ jsonError400 err
                  ( _ , _ , _, _,_,Just err)         -> do
                                                        logger INFO (show err )
                                                        throwError $ jsonError400 err
                  ( Nothing , Nothing, Nothing, Nothing, Nothing, Nothing ) -> return $ AT.Request logic upc
        else do
            logger INFO "Invalid date. Only future logic can be entered!"
            throwError $ jsonError400 "Invalid date. Only future logic can be entered!" 

-- | Function for Upc validation
validateUpc :: Integer -> Maybe String
validateUpc upc = if (upc >= 0 && upc < 1000000000000 ) then Nothing         
                  else Just ("Invalid UPC . It should be a positive Integer with a max limit of 12 digits " ++ show upc)


-- | Function fo rGeneralMarkup validation
validateGeneralMarkup :: AT.Rule -> Maybe String
validateGeneralMarkup rule = if (AT.markupBasisPoints rule >= 0 && AT.markupBasisPoints rule < 10000 ) 
                             then Nothing 
                             else Just ("Invalid markupBasisPoint" ++ show rule)

-- | Function for storeException validation
validateStoreExceptions :: [AT.StoreException] -> Maybe String
validateStoreExceptions strExcps = validateList strExcps
                                                condition
                                                "Invalid store or zone number "
  where
    condition :: AT.StoreException -> Bool
    condition =
        \(AT.StoreException rule store zone) -> ( store > 0 && store < 100000 ) &&
                                                (zone > 0 && zone < 100000 )


-- | Function for zoneException validation
validateZoneExceptions :: [AT.ZoneException] -> Maybe String
validateZoneExceptions znExcps = validateList znExcps condition "Invalid zone number "
  where
    condition :: AT.ZoneException -> Bool
    condition = \(AT.ZoneException rule zone) -> zone > 0 && zone < 100000


-- | Function for zoneMarkup validation
validateZoneMarkup :: [AT.ZoneException] -> Maybe String
validateZoneMarkup znExcps = validateList znExcps condition "Invalid zone markupBasisPoint "
  where
    condition :: AT.ZoneException -> Bool
    condition = \(AT.ZoneException rule zone) -> AT.markupBasisPoints rule> 0 && AT.markupBasisPoints rule< 10000

-- | Function for storeMarkup validation
validateStoreMarkup :: [AT.StoreException] -> Maybe String
validateStoreMarkup strExcps = validateList strExcps
                                                condition
                                                "Invalid store or zone markupBasisPoint "
  where
    condition :: AT.StoreException -> Bool
    condition =
        \(AT.StoreException rule store zone) -> AT.markupBasisPoints rule  > 0 && AT.markupBasisPoints rule < 10000 

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



                                            