{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module ShopperAPI.Services.Database.LoginDB where

import           ShopperAPI.Services.Database.Types
import           Database.PostgreSQL.Simple (Connection, query)

validLogin :: String -> String -> Connection -> IO (Either String LoginInfo)
validLogin uname password conn = do
      mgrData <- query conn "SELECT * FROM login WHERE\
      \ (email =? OR username=?) AND\
      \ password=?" [uname,uname, password] :: IO [LoginInfo]
      if Prelude.null mgrData
         then return (Left  "invalid credentials")
      else return $ Right $ head mgrData