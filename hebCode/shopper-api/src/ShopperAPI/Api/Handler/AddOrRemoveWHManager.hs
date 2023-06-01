{-|
    Module          : ShopperAPI.Api.Handler.AddOrRemoveWHManager
    Description     : Handler for /addwarehousemanager and for /removewarehousemanager
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module           ShopperAPI.Api.Handler.AddOrRemoveWHManager where

import           Control.Monad                      (mplus)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.ByteString                    hiding (any)
import           Data.Char                          (isDigit, isLower, isUpper)
import           Data.Maybe
import           GHC.Int
import           Servant
import           ShopperAPI.Api.Types
import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types
import           Text.Regex.TDFA

-- | Handler function to insert a WarehouseManager
insertWareHouseManager :: Credentials -> ApiHandler( ApiResponse String)
insertWareHouseManager user = do
    validateRequest <- validateEmailAndUsername user
    let mailid = email user
        usrnm = userName user
        paswd = userPassword user
        validatePass = validatePassword paswd
    case validatePass of
        Just err -> throwError $ jsonError400 err
        Nothing -> do
            dbOps  <- asks envDbOps
            checkIfUnique <- liftIO $ fetchUNameAndEmail dbOps mailid usrnm
            case checkIfUnique of
                Right [] -> checkPasswordEncryption dbOps
                                                    mailid
                                                    usrnm
                                                    paswd
                Right [(mailid', usrnm')] -> throwError $ jsonError500 "Username/Email already exists"
                Left err -> throwError $ jsonError500 "Failed to fetch data"

-- | Function to validate email and username
validateEmailAndUsername :: Credentials -> ApiHandler Credentials
validateEmailAndUsername credentials = do
    let validateResult = validateEmail (email credentials) `mplus`
                         validateUsername (userName credentials)
    case validateResult of
        Just err -> throwError $ jsonError400 err
        Nothing  -> return credentials

-- | Validates password encryption
checkPasswordEncryption :: DbOps -> String -> String -> String -> ApiHandler (ApiResponse String)
checkPasswordEncryption dbOps email'' password'' username'' = do
    encryptedPassword <- liftIO $ encryptPassword password''
    case encryptedPassword of
        Just encryptedPassword ->
            createWareHouseManager dbOps email'' username'' encryptedPassword
        Nothing -> do
            throwError $ jsonError500 "Encryption Failed"

-- | Function to insert new manager into database
createWareHouseManager :: DbOps -> String -> String -> ByteString -> ApiHandler(ApiResponse String)
createWareHouseManager dbops uemail uname upass  =
    liftIO (addWHouseManager dbops uemail uname upass ) >>= handleDbResponse
  where
    handleDbResponse :: Either DbError Int64 -> ApiHandler (ApiResponse String)
    handleDbResponse rowsUpdated = case rowsUpdated of
      Left err -> throwError $ jsonError500 "Updation failed, try again."
      Right y  -> do
        let message = "Account created for a new WareHouseManager"
        return $ ApiResponse message Nothing 200

-- | Handler function to remove warehousemanager with specified warehousemanagerid
removeWareHouseManager :: Int -> ApiHandler( ApiResponse String)
removeWareHouseManager wareHouseManagerid = do
    if wareHouseManagerid > 0
        then do
            dbOps <- asks envDbOps
            dbresult <- liftIO $  removeWHouseManag dbOps wareHouseManagerid
            case dbresult of
                Right 1 -> return $ ApiResponse "Deleted Successfully" Nothing 200
                Left msg -> throwError $ jsonError500 "Failed to Delete WareHouseManager"
        else throwError $ jsonError500 "Invalid warehousemanagerid"

-- | Function to validate email
validateEmail :: String -> Maybe String
validateEmail email =
    if email =~ ("[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+$" :: String)
        then Nothing
        else Just "Invalid email."

-- | Function to validate username
validateUsername :: String -> Maybe String
validateUsername userName =
    if userName =~ ("[a-zA-Z0-9]{6,30}$" :: String)
        then Nothing
        else Just "Please ensure username meets the requirements."

-- | Function to validate password
validatePassword :: String -> Maybe String
validatePassword password =
    if password =~ ("^[a-zA-Z0-9]{6,15}$"::String) &&  any isUpper password && any isLower password && any isDigit password
        then Nothing
        else Just "Please ensure the password meets the requirements."
