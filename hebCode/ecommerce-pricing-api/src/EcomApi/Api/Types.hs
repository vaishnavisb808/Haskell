{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}

module EcomApi.Api.Types where

import EcomApi.Core.Types

import           Servant
import           Servant.Auth.Server
import GHC.Generics
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy     hiding (pack)
import           Data.Text
import           Data.Text.Encoding
import           Network.Wai
import           Network.Wai.Handler.Warp

-- | Data type for JWT Authorization
data Users = Users
  { userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

instance FromJWT Users
instance ToJWT Users
instance ToJSON Users
instance FromJSON Users

-- | API Structure
type API = ViewLogicAPI :<|> ModifyLogicAPI


{- | API endpoint to view all logic(historic,present
     and future) for a given UPC by providing JWT
     authorization token.
     Token can be passed using auth [JWT] combinator
      as bearer token
-}
type ViewLogicAPI = "viewlogic"
                  :> Auth '[JWT] Users
                  :> Capture "upc" Int
                  :> Get '[JSON] String


{- | API endpoint to create or modify future logic for
     given upc by providing JWT
     authorization token.
     Token can be passed using auth [JWT] combinator
     as bearer token
-}
type ModifyLogicAPI = "modifylogic"
            :> Auth '[JWT] Users 
            :> ReqBody '[JSON] Logic 
            :> Post '[JSON] (ApiResponse String )


