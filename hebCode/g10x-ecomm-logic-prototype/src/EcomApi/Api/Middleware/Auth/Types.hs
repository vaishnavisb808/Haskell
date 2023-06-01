{-# LANGUAGE DeriveGeneric #-}
module EcomApi.Api.Middleware.Auth.Types where

import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics        (Generic)
import           Servant.Auth.Server (FromJWT, ToJWT)

-- | Data type for JWT Authorization
data AppData = AppData
  { appId   :: String
  , appName :: String
  } deriving (Eq, Show, Generic)

instance FromJWT AppData
instance ToJWT AppData
instance ToJSON AppData
instance FromJSON AppData
