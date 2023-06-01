{-|
    Module : ShopperAPI.Api.Handler.UserSignupHandler
    Description : Handler functions for /usersignup
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ShopperAPI.Api.Handler.UserSignupHandler where

import           Control.Monad                  ( mplus )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Control.Monad.Reader           ( asks )
import           Control.Monad.Trans.Maybe      ( MaybeT(MaybeT, runMaybeT) )
import           Data.ByteString                ( ByteString )
import           Data.Char                      ( isDigit
                                                , isLower
                                                , isUpper
                                                )
import           Data.Maybe                     ( fromMaybe )
import           GHC.Int                        ( Int64 )
import           Servant                        ( err500
                                                , errBody
                                                , throwError
                                                )
import           Text.Regex.TDFA                ( (=~) )

import           ShopperAPI.Api.Types 
import           ShopperAPI.Core.Config.Types
import           ShopperAPI.Core.Utils
import           ShopperAPI.Services.Database.Types
import           ShopperAPI.Services.Logger.Logger
                                                ( userSignupHandleLog
                                                , withTimeLog
                                                )
import           ShopperAPI.Services.Logger.Types
                                                ( EndPoint(UserSignupHandle)
                                                , LogLevel(ERROR, INFO)
                                                )

type Logger = LogLevel -> String -> Integer -> ApiHandler ()

{- | Handler function for /usersignup endpoint.
        1. Validates the input (email,username,password)
        2. Encrypt the password
-}

userSignupHandler :: Credentials -> ApiHandler (ApiResponse String)
userSignupHandler credentials = withTimeLog UserSignupHandle $ do
    let logger = userSignupHandleLog
    validateRequest <- validateEmailAndUsername credentials logger
    let email'            = email credentials
        username'         = userName credentials
        password'         = userPassword credentials
        validatePassword' = validatePassword password'
    case validatePassword' of
        Just err -> do
            logger INFO (show err) 400
            throwError $ jsonError400 err
        Nothing -> do
            dbOps       <- asks envDbOps
            uniqueCheck <- liftIO $ usernameAndemailcheck dbOps username' email'
            case uniqueCheck of
                Right [] -> checkPasswordEncryption dbOps
                                                    email'
                                                    password'
                                                    username'
                                                    logger
                Right [(email'', username'')] -> do
                    let logMessage = "Username/Email already exists"
                    logger INFO logMessage 500
                    throwError $ jsonError500 "Username/Email already exists"
                Left err -> do
                    logger INFO (show err) 500
                    throwError $ jsonError500 "Failed to fetch data"

-- | Validates email and username.                              
validateEmailAndUsername :: Credentials -> Logger -> ApiHandler Credentials
validateEmailAndUsername credentials logger = do
    let validateResult = validateEmail (email credentials) `mplus` 
                          validateUsername (userName credentials)
    case validateResult of
        Just err -> do
            logger INFO (show err) 400
            throwError $ jsonError400 err
        Nothing -> return credentials

-- | Validates password encryption
checkPasswordEncryption :: DbOps -> String -> String -> String -> Logger -> ApiHandler (ApiResponse String)
checkPasswordEncryption dbOps email'' password'' username'' logger = do
    encryptedPassword <- liftIO $ encryptPassword password''
    case encryptedPassword of
        Just encryptedPassword ->
            createUser dbOps email'' encryptedPassword username'' logger
        Nothing -> do
            let logMessage = "Encryption Failed"
            logger INFO logMessage 500
            throwError $ jsonError500 "Encryption Failed"

-- | Function to insert a new user
createUser :: DbOps -> String -> ByteString -> String -> Logger -> ApiHandler (ApiResponse String)
createUser dbOps userEmail userPassword userName logger =
    liftIO (userInsert dbOps userEmail userPassword userName) >>= handleDbResponse
  where
    handleDbResponse :: Either DbError Int64 -> ApiHandler (ApiResponse String)
    handleDbResponse rowsUpdated = case rowsUpdated of
        Left err -> do
            let logMessage = "Updation failed, try again."
            logger INFO logMessage 500
            throwError $ jsonError500 "Updation failed, try again."
        Right y -> do
            let message = "New user has been added"
            return $ ApiResponse message Nothing 200

-- | Function to validate email
validateEmail :: String -> Maybe String
validateEmail email =
    if email =~ ("[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+$" :: String)
        then Nothing
        else Just "Invalid email."

-- | Function to validate username
validateUsername :: String -> Maybe String
validateUsername username =
    if username =~ ("[a-zA-Z0-9@#$%^&-+=()_]{6,}$" :: String)
        then Nothing
        else Just "Username should contain minimum 6 characters."

-- | Function to validate password
validatePassword :: String -> Maybe String
validatePassword password =
    if password =~ ("^[a-zA-Z0-9]{8,}$" :: String) && any isUpper password && any isDigit password
        && any isLower password
        then Nothing
        else Just "Please ensure password requirements"

