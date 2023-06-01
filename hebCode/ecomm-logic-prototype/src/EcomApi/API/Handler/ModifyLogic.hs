{-|
    Module : EcomApi.API.Handler.ModifyLogic
    Description : Handler functions for /modifylogic
    Handler function for /modify logic
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module EcomApi.API.Handler.ModifyLogic where

import           EcomApi.Core.Utils
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Time
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField    (ToField (toField))
import           EcomApi.Core.Config.Types             (Env (envDbOps))
import           EcomApi.Core.Types                    as CT
import           EcomApi.API.Types                    as AT
import           EcomApi.Services.Database.ModifyLogic
import           EcomApi.Services.Database.Postgres
import           EcomApi.Services.Database.Types
import           GHC.Int
import           Servant
import           Servant.Server
import Data.Text.Internal.Encoding.Utf32            (validate)
import           EcomApi.Core.Transformers          (convertApiTypeToCoreType)



-- | Handler function for modifyLogic endpoint.
updateLogicHandler :: AT.Request ->ApiHandler (ApiResponse String)
updateLogicHandler request = do
    validateCheck <- validationRequest request
    case validateCheck of
        Right True -> do
            let logic= convertApiTypeToCoreType request
            let upc' = CT.upc logic
            dbOps <- asks envDbOps
            presentTime <- liftIO $ getCurrentTime
            upcData <- liftIO $ getupcData dbOps upc'
            case upcData of
                Right [] -> do
                     insertStatus<- liftIO $ insLogic dbOps logic True
                     case insertStatus of
                        Left e -> throwError $jsonError500 "Updation failed, try again."
                        Right y -> return $ ApiResponse "Inserted logic" Nothing 200
                Right [(effFrm,logicid)] -> do
                    if effFrm > presentTime
                        then modifyLogicHandler logic logicid
                        else do
                            insertStatus<- liftIO $ insLogic dbOps logic False
                            case insertStatus of
                                Left e -> throwError $jsonError500 "Updation failed, try again."
                                Right y -> return $ ApiResponse "Updated logic" Nothing 200
                Right _-> throwError $jsonError500 "Updation failed, try again." 
                Left err -> throwError $jsonError500  "Updation failed, try again."
        Right _-> throwError $jsonError500 "Updation failed, try again." 
        Left err -> throwError $jsonError500 err
    

-- | Handler function for modifyLogic endpoint when upc is present.
modifyLogicHandler :: CT.Logic ->Int-> ApiHandler (ApiResponse String)
modifyLogicHandler logic logicId = do
    dbOps <- asks envDbOps
    rowsUpdated <- liftIO $ modLogic dbOps logicId logic
    case rowsUpdated of
        Left e  -> throwError $jsonError500 "Updation failed, try again." 
        Right y -> return $ ApiResponse "Updated" Nothing 200



-- | Handler function for validating Request
validationRequest :: AT.Request -> ApiHandler(Either String Bool)
validationRequest (Request logic upc) = do
    let effectiveDate' = AT.effective logic
    presentTime <- liftIO $ getCurrentTime
    if effectiveDate' > presentTime
        then do
           let  storeException'= (\(Just storeException)->storeException) (storeExceptions logic)
           let storeCheck = storeExceptionCheck storeException'
           if storeCheck == True 
               then do
                    let  zoneException= (\(Just zoneExce)->zoneExce) (zoneExceptions  logic)
                    let zoneCheck =  zoneExceptionCheck zoneException
                    if zoneCheck == True 
                            then return $ Right True
                            else return $ Left "Invalid zone number!"
              else return $ Left "Invalid zone or store number!"
        else return $ Left "Invalid date. Only future logic can be entered!"


-- | Function for storeException validation
storeExceptionCheck :: [AT.StoreException] -> Bool
storeExceptionCheck [] = True
storeExceptionCheck ((AT.StoreException rule store zone):xs) = do
    if (store > 0 && store < 100000 ) && (zone > 0 && zone < 100000 )
        then storeExceptionCheck xs  
        else False 

-- | Function for zoneException validation
zoneExceptionCheck :: [AT.ZoneException] -> Bool
zoneExceptionCheck [] = True
zoneExceptionCheck ((AT.ZoneException rule zone):xs) = do
    if (zone > 0 && zone < 100000 )
        then zoneExceptionCheck xs  
        else False 

