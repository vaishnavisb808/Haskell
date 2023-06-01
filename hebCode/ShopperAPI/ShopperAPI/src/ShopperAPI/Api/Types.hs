{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module           ShopperAPI.Api.Types where

import           GHC.Generics                      (Generic)
import           Servant                           (Capture, FromHttpApiData,
                                                    Get, Handler, JSON, Post,
                                                    QueryParam, ReqBody,
                                                    parseUrlPiece,BasicAuth,
                                                    (:<|>) ((:<|>)), (:>))
import           Servant.Auth.Server               (Auth, JWT, FromJWT, ToJWT)
import           Text.Read                         (readMaybe)

import           Data.ByteString.Lazy              hiding (pack, unpack)
import           Data.Text                         (unpack)
import           Data.Time                         (Day, UTCTime (UTCTime),
                                                    secondsToDiffTime, utctDay)

import           Control.Monad.Trans.Reader        (ReaderT)
import           Data.Aeson                        (FromJSON, ToJSON,
                                                    Value (Object), object,
                                                    parseJSON, toJSON, (.:),
                                                    (.=))

import          ShopperAPI.Core.Config.Types
import          Servant


type ApiHandler = ReaderT Env Handler

--Payload for the token generation
data AccessData = AccessData {
                          id   :: Integer ,
                          role :: String
                       }deriving (Eq,Show,Generic)

instance FromJSON AccessData
instance ToJSON AccessData

instance FromJWT AccessData
instance ToJWT AccessData


{-| response schema for all API endpoints
    This type is only used for successfull responses
    For failure scenarios custom formatters are used
-}
data ApiResponse a = ApiResponse
    { result :: a
    , error  :: Maybe Int
    , code   :: Int
    }deriving (Show,Generic)

instance (ToJSON a)=>ToJSON (ApiResponse a)
instance (FromJSON a)=>FromJSON (ApiResponse a)


type API  = AcceptOrRejectOrder
type AcceptOrRejectOrder = "orderstatus"
                         :> Auth    '[JWT] AccessData
                         :> Capture "orderid" Int
                         :> ReqBody '[JSON] OrderAccept
                         :> Post    '[JSON] (ApiResponse String)