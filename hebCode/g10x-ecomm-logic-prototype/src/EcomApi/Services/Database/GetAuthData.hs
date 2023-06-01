{-# LANGUAGE OverloadedStrings #-}

module EcomApi.Services.Database.GetAuthData where

import           EcomApi.Services.Database.Types ()

import           Control.Monad.Catch             (SomeException, handle)
import           Database.PostgreSQL.Simple      (Connection, query)
import           EcomApi.Core.Types              (UserInfo)

-- | retrieve the appId and appName for the curresponding User
getAuthData :: String -> Connection -> IO (Either String UserInfo)
getAuthData uname conn = handle handler $ do
      appData <- query conn "SELECT * FROM auth \
                            \WHERE app_id = ?" [uname] :: IO [UserInfo]
      if Prelude.null appData
         then return (Left  "invalid credentials")
      else return $ Right $ head appData
   where
      handler::SomeException->IO (Either String UserInfo)
      handler er = return (Left $ show er)
