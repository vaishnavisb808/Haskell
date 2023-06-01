{-|
    Module : ShopperAPI.Api.Handler.AddAddressHandlers
    Description : Handler functions for /addaddress
    Handler function for /adding user address
-}

{-# LANGUAGE OverloadedStrings #-}


module           ShopperAPI.Api.Handler.AddAddressHandlers where

import           Control.Monad                        (MonadPlus (mplus))
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Reader           (asks)
import           Servant                              (throwError)
import           ShopperAPI.Api.MiddleWare.Auth.Types as MT (AccessData (id))
import           ShopperAPI.Api.Types                 (Address (cityAndState, zipcode),
                                                       ApiHandler,
                                                       ApiResponse (ApiResponse))
import           ShopperAPI.Core.Config.Types         (Env (envDbOps))
import           ShopperAPI.Core.Transformers         (convertAddressApiToCore)
import           ShopperAPI.Core.Utils                (jsonError400,
                                                       jsonError500)
import           ShopperAPI.Services.Database.Types   (DbOps (insAddress, zipCheck))
import           ShopperAPI.Services.Logger.Logger    (addAddressHandleLog,
                                                       withTimeLog)
import           ShopperAPI.Services.Logger.Types     (EndPoint (AddAddressHandle),
                                                       LogLevel (ERROR, INFO))


type Logger = LogLevel -> String -> Integer -> ApiHandler ()

{- | Handler function for /addaddress endpoint.
        1. Validate request
        3. If the zipcode is valid, add a new address to the database.
-}
addAddress :: Address -> AccessData  -> ApiHandler( ApiResponse String)
addAddress request accessData= withTimeLog AddAddressHandle $ do
        let logger = addAddressHandleLog
        validateRequest request logger
        dbOps <- asks envDbOps
        zipresult <- liftIO $ zipCheck dbOps (zipcode request)
        case zipresult of
            Right False -> do
                let logMessage= "The zipcode is not in a serviceable location "
                logger INFO logMessage 400
                throwError $ jsonError400 "The zipcode is not in a serviceable location."
            Right True -> do
                let userid =MT.id accessData :: Int
                    convertAddressToCore = convertAddressApiToCore request
                dbresult <- liftIO $ insAddress dbOps convertAddressToCore userid
                case dbresult of
                    Right msg -> do
                        let logMessage= "New address has been saved for " ++ show userid
                        logger INFO logMessage 200
                        return $ ApiResponse "New address has been saved" Nothing 200
                    Left err -> do
                        logger ERROR (show err) 500
                        throwError $ jsonError500 "Insertion failed, try again."
            Left err -> do
                logger ERROR (show err) 500
                throwError $ jsonError500 "Insertion failed, try again."

-- |validate request body for add address
validateRequest:: Address -> Logger ->ApiHandler Address
validateRequest request logger =do
    let validationResult =  validateCityState (cityAndState request) `mplus`
                            validateZipcode (zipcode request)
    case validationResult of
        Just err -> do
            logger INFO  (show err) 400
            throwError $ jsonError400 err
        Nothing  -> return request

-- | validating whether city and state is supprated with coma
validateCityState :: String -> Maybe String
validateCityState cityState = if ',' `elem` cityState
                     then Nothing
                     else Just "Invalid city and state. City and state must be in the format city,state."

-- | validating whether zipcode is 5 digit and positive
validateZipcode::Integer -> Maybe String
validateZipcode zipcode = do
    let zipodelength = numDigits zipcode
    if zipodelength == 5
                     then Nothing
                     else Just "Invalid zipcode, Zipcode must be positive 5 digit "

-- | function for finding the digits
numDigits :: Integer -> Integer
numDigits n = toInteger (round (logBase 10 (fromIntegral n)) + 1)
