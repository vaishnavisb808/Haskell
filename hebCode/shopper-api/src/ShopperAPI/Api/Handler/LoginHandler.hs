
{-|
    Module : ShopperAPI.Api.Handler.ViewLogic
    Description : Handler for /login
-}

{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

module ShopperAPI.Api.Handler.LoginHandler (loginHandler) where

import           Control.Monad.IO.Class                         (MonadIO (liftIO))
import           Control.Monad.Trans.Reader                     (asks)
import           Crypto.BCrypt                                  (validatePassword)
import           Data.ByteString.Char8                          as P (pack,
                                                                      unpack)
import qualified Data.ByteString.Lazy.UTF8                      as B
import           Data.Maybe
import qualified Data.Text                                      as T
import           Data.Text.Encoding                             (encodeUtf8)
import           Data.Time
import           GHC.Generics
import           Servant                                        (throwError)
import           Servant.Auth.Server                            ()
import           ShopperAPI.Api.MiddleWare.Auth.TokenGeneration (generateToken)
import           ShopperAPI.Api.Types                           as AT (ApiHandler,
                                                                       ApiResponse (ApiResponse),
                                                                       Login (..))
import           ShopperAPI.Core.Config.Types                   (Env (envDbOps))
import           ShopperAPI.Core.Types                          as CT
import           ShopperAPI.Core.Utils                          (jsonError400,
                                                                 jsonError401,
                                                                 jsonError404)
import           ShopperAPI.Services.Database.Types             (DbError (dbError),
                                                                 DbOps (getLoginDatafromDB))
import           Text.Regex.TDFA                                ((=~))

-- | Handler function for login endpoint.
loginHandler :: Login -> ApiHandler( ApiResponse String)
loginHandler request =do
    dbOps <- asks envDbOps
    let usernameOrEmail = AT.usernameOrEmail request
        passwd = AT.password request
    currentDay <- liftIO (utctDay <$> getCurrentTime)
        -- differentiate the username and email
    let isUsernameOrEmail =  differentiateUsernameOrEmail usernameOrEmail
    case isUsernameOrEmail of
        Nothing         -> throwError $jsonError400 "Invalid username or email"
        Just credential -> do
            -- fetching all details according to the username or email
            userDetails <- liftIO $ getLoginDatafromDB dbOps credential
            case userDetails of
                Left msg -> throwError $jsonError400 "Invalid username or email"
                Right details -> do
                    let encryptedPassword = P.pack $ CT.password details
                    -- validating original password with encrypted password
                        isValidPassword = validatePassword encryptedPassword
                                                            (P.pack passwd)
                        passwordCreatedDay = utctDay (CT.time details)
                        isValidTime = validateDayDifference currentDay passwordCreatedDay
                        status = (isValidPassword,isValidTime)
                        tokenResponse token = return $ApiResponse token Nothing 200
                    case status of
                        (True,True) -> tokenResponse =<<generateToken details
                        (False,_) -> throwError $jsonError400 "Login Failed: Incorrect Password"
                        (_,False) -> throwError $jsonError400 "Login Failed: Password Expired"

-- | Function for differentiating the username and email from user input.
differentiateUsernameOrEmail :: String -> Maybe (String,Bool)
differentiateUsernameOrEmail usernameOrEmail = do
    if usernameOrEmail =~ "[a-zA-Z0-9+._-]+@[a-zA-Z0-9-]+\\.[a-z]+"
    then
        return (usernameOrEmail,True)
    else if usernameOrEmail =~ "^[A-Za-z0-9]+$"
    then
        return (usernameOrEmail,False)
    else Nothing

-- | Function for check the number of days between password generated date and current date.
validateDayDifference :: Day -> Day -> Bool
validateDayDifference currentDay createdDay = dateDifference < 90
  where
    dateDifference = diffDays currentDay createdDay
