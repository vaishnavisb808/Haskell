{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}


module EcomApi.Services.Database.Types where

import qualified Control.Exception.Base as EB

data DbError = DbError {
    dbError::String 
    }deriving (Eq,Show)
instance EB.Exception DbError
