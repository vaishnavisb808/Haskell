{-|
    Module : EcomApi.Api.Types
    Description : Types used for api request and response interpretation

    Types modelling the API request and response structure so as to
    conviniently parse and encode json data.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module EcomApi.Api.Types where

import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy            hiding (pack,unpack)
import           Data.Maybe
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           EcomApi.Core.Config.Types
import           EcomApi.Services.Database.Types
import           GHC.Generics
import           Network.Wai                     hiding (Request, Response)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server
import           Text.Read


-- | Custom monad for sharing variables across handlers
type ApiHandler = ReaderT Env Handler


-- | Request data type for modify endpoint
data Request = Request
 { futureLogic :: Logic   -- ^ Logic to be modified
 , upc         :: Integer -- ^ Universal Product Code for each product
} deriving (Generic)
instance FromJSON Request


-- | Response data type for view endpoint
data Response = Response
 { futureLogic     :: Maybe Logic    -- ^ Future logic of the upc
 , presentLogic    :: Maybe Logic          -- ^ Present logic of the upc
 , historicalLogic :: Maybe [Logic]  -- ^ List of hiostorical logics of the upc
 , upc             :: Integer        -- ^ Universal Product Code for each product
 }deriving (Generic ,Show)

instance ToJSON Response
instance FromJSON Response


-- | Data type for a Logic
data Logic = Logic
   { storeExceptions :: Maybe [StoreException]    -- ^ List of store exceptions of a logic id
   , zoneExceptions  :: Maybe [ZoneException]     -- ^ List of zone exceptions of a logic id
   , rule            :: Rule                -- ^ Rule details of a logic id
   , effective       :: UTCTime             -- ^ Date from which the logic is effective
   }  deriving (Generic ,Show)

instance ToJSON Logic where
    toJSON (Logic storeExceptions zoneExceptions rule effective) =
       object    ["storeExceptions" .= storeExceptions
                , "zoneExceptions" .=  zoneExceptions
                , "rule" .= rule
                , "effective" .= utcToDateString effective]
    

utcToDateString :: UTCTime -> String 
utcToDateString utcTime  = show 
                            (utctDay utcTime)
instance FromJSON Logic where
    parseJSON (Object v)= do
      date<- dateStringToUTCTime <$> (v .: "effective")
      case date of
        Just d-> Logic
          <$> (v .: "storeExceptions")
          <*> (v .: "zoneExceptions")
          <*> (v .: "rule")
          <*> return (d)
        Nothing ->fail "Invalid date"


{-|  Convert a date format string (eg: "2022-01-15") to  UTCTime
     the date is interpreted as a Day type and time is set to zero
     This is done so that the system has the capability to adopt time accurate date in
     future.
-}
dateStringToUTCTime :: String -> Maybe UTCTime
dateStringToUTCTime str =  do
  case (readMaybe str ::Maybe Day) of
    Just day -> Just $ UTCTime day (secondsToDiffTime 0)
    Nothing  -> Nothing


-- | Data type for a rule
data Rule = Rule
 { adjustmentMethod     :: String
 , ignoreClearancePrice :: String
 , noMarkupIfOnAd       :: String
 , markupBasisPoints    :: Integer
 }deriving (Generic ,Show)
instance ToJSON Rule
instance FromJSON Rule



-- | Data type for store exception
data StoreException = StoreException
  { rule  :: Rule            -- ^ Rule details of the particular store exception
  , store :: Integer         -- ^ Store number
  , zone  :: Integer         -- ^ Zone number
  } deriving (Generic ,Show)

instance ToJSON StoreException
instance FromJSON StoreException


-- | Data type for zone exception
data ZoneException = ZoneException
 { rule :: Rule           -- ^ Rule details of the particular zone exception
 , zone :: Integer        -- ^ Zone number
 }deriving (Generic ,Show)

instance ToJSON ZoneException
instance FromJSON ZoneException

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


-- | Data type for JWT Authorization
data AppData = AppData
  { appId   :: String
  , appName :: String
  } deriving (Eq, Show, Generic)

instance FromJWT AppData
instance ToJWT AppData
instance ToJSON AppData
instance FromJSON AppData

newtype UPC = UPC Integer deriving Show

instance FromHttpApiData UPC where
  parseUrlPiece upctxt = case (readMaybe (unpack upctxt) :: Maybe Integer) of 
                              (Just x) ->Right (UPC x)
                              Nothing -> Left "Incorrect UPC"


-- | API Structure
type API = ViewLogicAPI :<|> ModifyLogicAPI


{-| API endpoint to view all logic(historic,present
     and future) for a given UPC by providing JWT
     authorization token.
     Token can be passed using as bearer token
-}
type ViewLogicAPI = "viewlogic"
                  :> Auth '[JWT] AppData
                  :> Capture "upc" UPC
                  :> QueryParam "page" Integer
                  :> Get '[JSON] (ApiResponse Response)


{-| API endpoint to create or modify future logic for
     given upc by providing JWT
     authorization token.
     Token can be passed using auth [JWT] combinator
     as bearer token
-}
type ModifyLogicAPI = "modifylogic"
            :> Auth '[JWT] AppData
            :> ReqBody '[JSON] Request
            :> Post '[JSON] (ApiResponse String)
