{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}

module ShopperAPI.Core.Types      where

import           Data.Aeson
import           Data.Time                            (UTCTime)
import           Database.PostgreSQL.Simple           (FromRow,
                                                       ResultError (ConversionFailed),
                                                       ToRow)
import           Database.PostgreSQL.Simple.FromField (FromField (fromField),
                                                       ResultError, returnError)
import           Database.PostgreSQL.Simple.ToField   (ToField (toField))
import           GHC.Generics                         (Generic)
import           Servant.Auth.Server                  (FromJWT, ToJWT)
import           Text.Read                            (readMaybe)


-- | Datatype for adding product to database
data AddProduct = AddProduct
  {   productName        :: String
  ,   category           :: String
  ,   productPrice       :: Float
  ,   currency           :: String
  ,   productDescription :: String
  ,   status             :: String
  }   deriving (Eq, Show, Generic)

instance FromJSON AddProduct
instance ToJSON   AddProduct
instance FromRow  AddProduct
instance ToRow    AddProduct

data Tracking = Tracking
      { orderid :: Int
      , refNo   :: String
      , date    :: UTCTime
     } deriving (Generic, Show)

instance FromJSON Tracking where
    parseJSON (Object v)= do
      maybeDate <- parseDate <$> (v .: "date")
      case maybeDate of
          Nothing -> fail "Invalid"
          Just y -> do
            let date = y
            Tracking <$> (v .: "orderid") <*> (v .: "refNo") <*> return date
    parseJSON _ = fail "Invalid date"

data AcceptOrReject = Accept
                     | Reject
                     | InProgress
    deriving (Show, Generic, Eq)

instance FromField AcceptOrReject where
    fromField f acceptOrReject = case acceptOrReject of
      Just "Accept"     -> return Accept
      Just "Reject"     -> return Reject
      Just "InProgress" -> return InProgress
      _                 -> returnError  ConversionFailed f "Invalid Order Status"

instance ToField AcceptOrReject where
    toField Accept     = toField $ show Accept
    toField Reject     = toField $ show Reject
    toField InProgress = toField $ show InProgress

newtype OrderAccept = OrderAccept {
    status :: AcceptOrReject
}

-- | Function to convert string to utctime
parseDate :: String -> Maybe UTCTime
parseDate x = if x /= "" then (readMaybe x :: Maybe UTCTime) else Nothing


data LoginInfo = LoginInfo
  { login_id :: Int
  , email    :: String
  , username :: String
  , password :: String
  , role     :: String
  , time     :: UTCTime
  } deriving (Eq, Show, Generic)
instance FromRow LoginInfo
instance ToRow   LoginInfo

-- | Data type for Address
data Address = Address {
      recipientFirstName  :: String
    , recipientLastName   :: String
    , addressLine1        :: String
    , addressLine2        :: String
    , landMark            :: String
    , cityAndState        :: String
    , zipcode             :: Integer
    , country             :: String
    , communicationNumber :: Integer
}deriving (Show,Generic)

instance ToJSON   Address
instance FromJSON Address

-- | Data type for ProductStock
data ProductStock = ProductStock
 { productId  :: Int
 , stockCount :: Int
 } deriving(Show,Generic)

instance FromJSON ProductStock
instance ToJSON   ProductStock

data AddTempValueInfo'= AddTempValueInfo'
    {
          addProductId          :: Integer
        , addTempValue          :: Float
        , addTempCurrecy        :: String
        , addEffectiveStartDate :: UTCTime
        , addEffectiveEndDate   :: UTCTime

    } deriving (Generic, Show, Eq)
instance ToRow   AddTempValueInfo'
instance FromRow AddTempValueInfo'

-- | Response data type for viewcart endpoint
data ProductCart = ProductCart
 { productname  :: String
 , price        :: Float
 , currency     :: String
 , availability :: String
 , description  :: String
 , cartquantity :: Int
 , updatedat    :: UTCTime
 } deriving (Eq, Show, Generic, FromRow)
instance FromJSON ProductCart
instance ToJSON   ProductCart

-- | Data type for each product
data ProductDetailsWithId = ProductDetailsWithId
    { productName        :: String
    , productId          :: Int
    , productDescription :: String
    , category           :: String
    , productPrice       :: Float
    , currency           :: String
    , temporaryPrice     :: Maybe Float
    , numberOfReviews    :: Int
    , rating             :: Float
    , status             :: String
    , images             :: Maybe [String]
    } deriving (Show, Eq, Generic)
instance ToJSON   ProductDetailsWithId
instance FromJSON ProductDetailsWithId

-- | Data type for view WishList
data WishList = WishList
   { productName          :: String
    , productPrice        :: Float
    , currency            :: String
    , productAvailability :: String
   } deriving (Show, Generic)

instance FromJSON WishList
instance ToJSON   WishList
instance FromRow  WishList

-- | Data type for each product
data ProductDetails = ProductDetails {
      productName        :: String
    , productDescription :: String
    , category           :: String
    , productPrice       :: Float
    , currency           :: String
    , temporaryPrice     :: Maybe Float
    , numberOfReviews    :: Int
    , rating             :: Float
    , images             :: Maybe [String]
 }deriving (Show, Eq, Generic)
instance FromJSON ProductDetails
instance ToJSON   ProductDetails

data ProductResponse = ProductResponse
 {
      products :: [ProductDetails]
    , pageKey  :: Maybe Int
 } deriving (Show, Eq, Generic)

instance ToJSON   ProductResponse
instance FromJSON ProductResponse
-- | Data type for edit a product
data AddProduct' = AddProduct'
    {
        productName'  :: String
    ,   category'     :: Int
    ,   productPrice' :: Float
    ,   currency'     :: String
    ,   description'  :: String
    ,   status'       :: String
    ,    productId'   :: Int
    }
data EditProduct = EditProduct
    {
        productName'  :: String
    ,   category'     :: Int
    ,   productPrice' :: Float
    ,   currency'     :: String
    ,   description'  :: String
    ,   status'       :: String
    ,   productId'    :: Int
    }deriving (Eq, Show, Generic)

instance FromRow  EditProduct
instance ToRow    EditProduct
instance FromJSON EditProduct
instance ToJSON   EditProduct

newtype Status = Status {
   status' :: String
}deriving (Show , Generic)

instance FromJSON Status
instance ToRow    Status
instance ToJSON   Status
instance FromRow  Status

newtype Category' = Category'
    {   categoryId' :: Int
    }deriving (Show,Eq,Generic)
instance FromRow Category'


-- | Data type for AddComments
data AddComments = AddComments
 { productId  :: Int
 , comment :: String
 } deriving(Show,Generic)

instance FromJSON AddComments
instance ToJSON AddComments
