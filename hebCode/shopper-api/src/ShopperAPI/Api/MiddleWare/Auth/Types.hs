{-|
    Module      : ShopperAPI.Api.MiddleWare.Auth.Types
    Description : Types used for api authentication
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module           ShopperAPI.Api.MiddleWare.Auth.Types where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.ByteString
import           Database.PostgreSQL.Simple.FromField (FromField (fromField))
import           GHC.Generics                         (Generic)
import           Servant.Auth.Server                  (FromJWT, ToJWT)

-- | Payload for the token generation
data AccessData = AccessData
    { id   :: Int
    , role :: Role
    } deriving (Show, Generic,Read)

instance ToJWT    AccessData
instance FromJWT  AccessData
instance ToJSON   AccessData
instance FromJSON AccessData

-- | Data type for Role
data Role = User
          | WareHouseManager
          | Manager deriving (Eq,Show,Generic,Read)

instance FromJSON Role
instance ToJSON Role

instance FromField Role where
    fromField f mngr = case mngr of
        Just "User"             -> return User
        Just "WareHouseManager" -> return WareHouseManager
        Just "Manager"          -> return Manager
        _                       -> Prelude.error "error"
