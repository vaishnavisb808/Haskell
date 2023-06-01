{- |
    Module      : ShopperAPI.Api.Types
    Description : Types used for api request and response interpretation
    Types modelling the API request and response structure so as to
    conviniently parse and encode json data.
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}


module           ShopperAPI.Api.Types where

import           Control.Monad.Trans.Reader           (ReaderT)
import           Data.Aeson                           (FromJSON, ToJSON,
                                                       Value (Object), object,
                                                       parseJSON, toJSON, (.:),
                                                       (.=))
import           Data.ByteString
import           Data.ByteString.Lazy                 hiding (pack, unpack)
import           Data.Text
import           Data.Time                            (Day, UTCTime (UTCTime),
                                                       secondsToDiffTime,
                                                       utctDay)
import           Database.PostgreSQL.Simple           (FromRow, ToRow)
import qualified Database.PostgreSQL.Simple.FromField as FF
import qualified Database.PostgreSQL.Simple.ToField   as TF
import           GHC.Generics                         (Generic)
import           Servant                              (BasicAuth, Capture,
                                                       Delete, FromHttpApiData,
                                                       Get, Handler, JSON, Post,
                                                       QueryParam, ReqBody,
                                                       parseUrlPiece,
                                                       (:<|>) ((:<|>)), (:>))
import           Servant.API                          (FromHttpApiData (parseQueryParam))
import           Servant.Auth.Server                  (Auth, FromJWT, JWT,
                                                       ToJWT)
import           ShopperAPI.Api.MiddleWare.Auth.Types
import           ShopperAPI.Core.Config.Types         (Env)
import           Text.Read                            (readMaybe)

-- | Custom monad for sharing variables across handlers
type ApiHandler = ReaderT Env Handler

data ProductCart = ProductCart
 { productname  :: String
 , price        :: Float
 , currency     :: String
 , availability :: String
 , description  :: String
 , cartquantity :: Int
 , updatedat    :: UTCTime
 } deriving (Eq, Show, Generic,FromRow)
instance FromJSON ProductCart
instance ToJSON   ProductCart

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

-- | Data type for AddComments
data AddComments = AddComments
 { productId :: Int
 , comment   :: String
 } deriving(Show,Generic)

instance FromJSON AddComments
instance ToJSON AddComments

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

instance ToJSON Address
instance FromJSON Address

-- | Response data type for getproducts endpoint
data Response = Response
    { productDetails :: [ProductDetailsWithId]
    , offset         ::Maybe Integer
    } deriving (Show, Eq, Generic)

instance ToJSON Response
instance FromJSON Response

-- | Function to convert string to utctime
parseDate :: String -> Maybe UTCTime
parseDate x = if x /= "" then (readMaybe x :: Maybe UTCTime) else Nothing

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

{-| response schema for all API endpoints
    This type is only used for successfull responses
    For failure scenarios custom formatters are used
-}
data ApiResponse a = ApiResponse
  { result :: a
  , error  :: Maybe String
  , code   :: Int
  } deriving (Show, Generic)

instance (ToJSON a)   => ToJSON   (ApiResponse a)
instance (FromJSON a) => FromJSON (ApiResponse a)

-- | Datatype for adding product to databse
data AddProduct = AddProduct
  {   productName        :: String
  ,   category           :: String
  ,   productPrice       :: Float
  ,   currency           :: String
  ,   productDescription :: String
  ,   status             :: String
  }deriving (Eq, Show, Generic)
instance FromRow  AddProduct
instance ToRow    AddProduct
instance FromJSON AddProduct
instance ToJSON   AddProduct


-- | Datatype for login credentials
data Credentials = Credentials  {
      email        :: String
    , userPassword :: String
    , userName     :: String
}deriving (Show,Generic)

instance FromJSON Credentials
instance ToJSON   Credentials

-- | Data type for the Login credentials
data Login = Login
    {   usernameOrEmail :: String
    ,   password        :: String
    }deriving (Eq,Show,Generic)

instance FromJSON Login
instance ToJSON   Login


-- datatype for editing the product details
data EditProduct = EditProduct
 {
      productId    :: Int
    , productName  :: String
    , category     :: String
    , productPrice :: Float
    , currency     :: String
    , description  :: String
    , status       :: String
 }deriving (Eq, Show, Generic)

instance FromJSON EditProduct
instance ToJSON   EditProduct
instance ToRow    EditProduct
instance FromRow  EditProduct

data AddTempValueInfo= AddTempValueInfo
  {
    addProductId          :: Integer
  , addTempValue          :: Float
  , addTempCurrecy        :: String
  , addEffectiveStartDate :: UTCTime
  , addEffectiveEndDate   :: UTCTime
  } deriving (Generic, Show, Eq)

instance FromRow  AddTempValueInfo
instance ToRow    AddTempValueInfo
instance ToJSON   AddTempValueInfo
instance FromJSON AddTempValueInfo where
    parseJSON (Object v) = do
      dateTime <- dateStringToUTCTime <$> (v .: "addEffectiveStartDate")
      case dateTime of
        Nothing -> fail "Invalid date"
        Just dt -> do
          date <- dateStringToUTCTime <$> (v .: "addEffectiveEndDate")
          case date of
            Just d  -> AddTempValueInfo
              <$> (v .: "addProductId")
              <*> (v .: "addTempValue")
              <*> (v .: "addTempCurrecy")
              <*> return d
              <*> return dt
            Nothing -> fail "Invalid date"
    parseJSON _ = fail "Invalid date"


{-|  Convert a date format string (eg: "2022-01-15") to  UTCTime
     the date is interpreted as a Day type and time is set to zero
     This is done so that the system has the capability to adopt time accurate date in
     future.
-}
dateStringToUTCTime :: String -> Maybe UTCTime
dateStringToUTCTime str =  do
  case (readMaybe str :: Maybe Day) of
    Just day -> Just $ UTCTime day (secondsToDiffTime 0)
    Nothing  -> Nothing

-- | Data type for ProductStock
data ProductStock = ProductStock
 { productId  :: Int
 , stockCount :: Int
 } deriving(Show,Generic)

instance FromJSON ProductStock
instance ToJSON   ProductStock

data ProductResponse = ProductResponse
 {
      products :: [ProductDetails]
    , pageKey  :: Maybe Int
 } deriving (Show, Eq, Generic)

instance ToJSON   ProductResponse
instance FromJSON ProductResponse

data AcceptOrReject = Accept
                     | Reject
                     | InProgress
    deriving (Show, Generic, Eq)
instance FromHttpApiData AcceptOrReject where
    parseQueryParam status = case status of
                               "Accept" -> Right  Accept
                               "Reject" -> Right  Reject
                               _        -> Left "error"

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

-- | API Structure
type API = AddCategory :<|> Removecategory :<|> MarkAsUnavilable :<|> UpdateStock
    :<|> UserSignUp :<|>  ViewProductsByRating :<|> AddAddress :<|> LoginApi
    :<|> EditProducts :<|> RemoveProduct :<|> ViewProducts :<|> AddWarehouseManager
    :<|> RemoveWarehouseManager :<|> MarkAsShipped :<|> ViewCart :<|> AddProducts
    :<|> AddToCart :<|> RemoveCart :<|> AcceptOrRejectOrder :<|> TempPriceChange
    :<|> AddComment  :<|> ViewWishlist :<|> CartQuantityUpdate

{-| API endpoint to update the product status for a given product id by providing JWT
     Token can be passed using as bearer token
-}
type MarkAsUnavilable = "products"
                      :> "markasunavilable"
                      :> Auth    '[JWT] AccessData
                      :> Capture "productid" Int
                      :> Post    '[JSON] (ApiResponse String)

{-| API endpoint to add product to the inventory by providing JWT
    authorization token
    Token can be passed  using as bearer token -}
type AddProducts = "addproduct"
                :> Auth    '[JWT] AccessData
                :> ReqBody '[JSON] AddProduct
                :> Post    '[JSON] (ApiResponse String)

{-| API endpoint to add Category providing JWT
     authorization token.
     Token can be passed using as bearer token
-}
type AddCategory = "addcategory"
                :> Auth '[JWT] AccessData
                :> QueryParam "category" String
                :> Post '[JSON](ApiResponse String)


{-| API endpoint to Remove Category providing JWT
     authorization token.
     Token can be passed using as bearer token
-}
type Removecategory = "removecategory"
                    :> Auth   '[JWT] AccessData
                    :> Capture "categoryid" Integer
                    :> Delete '[JSON] (ApiResponse String)

{-| API endpoint to add the shipping status for a given order id by providing JWT
     authorization token.
     Token can be passed using as bearer token
-}
type MarkAsShipped = "markasshipped"
                  :> Auth    '[JWT] AccessData
                  :> ReqBody '[JSON] Tracking
                  :> Post    '[JSON] (ApiResponse String)

type UpdateStock = "updatingstock"
                :> Auth    '[JWT] AccessData
                :> ReqBody '[JSON] ProductStock
                :> Post    '[JSON] (ApiResponse String)

{-| API endpoint for the user to signup using email, username and password. -}
type UserSignUp = "usersignup"
                :> ReqBody '[JSON] Credentials
                :> Post    '[JSON] (ApiResponse String)

{- | API endpoint to add warehousemanager by providing JWT authorization token.
     Token can be passed using bearer token.
-}
type AddWarehouseManager = "addwarehousemanager"
                        :> Auth    '[JWT] AccessData
                        :> ReqBody '[JSON] Credentials
                        :> Post    '[JSON] (ApiResponse String)

{- | API endpoint to remove warehousemanager for a given warehousemanagerid by providing JWT authorization token.
     Token can be passed using bearer token.-}
type RemoveWarehouseManager = "removewarehousemanager"
                            :> Auth    '[JWT] AccessData
                            :> Capture "warehousemanagerid" Int
                            :> Delete  '[JSON] (ApiResponse String)

{-| API endpoint to view all products
    based on sortBy  by providing JWT
    authorization token.
    Token can be passed using as bearer token
-}
type ViewProductsByRating = "products"
                         :> Auth '[JWT] AccessData
                         :> QueryParam "sortBy" String
                         :> QueryParam "offset" Integer
                         :> Get  '[JSON] (ApiResponse Response)

type AddAddress = "addaddress"
                :> Auth    '[JWT]  AccessData
                :> ReqBody '[JSON] Address
                :> Post    '[JSON] (ApiResponse String)

{-| API endpoint to add comment for a product by providing JWT
    authorization token.Token can be passed using as bearer token
-}
type AddComment = "addcomment"
                :> Auth    '[JWT] AccessData
                :> ReqBody '[JSON] AddComments
                :> Post    '[JSON] (ApiResponse String)

type AddToCart = "addproducttocart"
               :> Auth '[JWT] AccessData
               :> Capture "productid" Int
               :> Post '[JSON] (ApiResponse String)


type RemoveCart = "removefromcart"
                :> Auth   '[JWT] AccessData
                :> Capture "productid" Int
                :> Delete '[JSON] (ApiResponse String)


{-| API endpoint to view products in the cart of a user by providing JWT
     authorization token.
     Token can be passed using as bearer token
-}
type ViewCart = "viewcart"
              :> Auth '[JWT] AccessData
              :> Get  '[JSON] ( ApiResponse [ProductCart])

-- | Endpoint for viewing products by the user.
type ViewProducts = "viewproducts"
                  :> Auth '[JWT] AccessData
                  :> QueryParam "category" String
                  :> QueryParam "lastid" Int
                  :> Get '[JSON] ( ApiResponse ProductResponse)


{-| API endpoint to login. After the succeful
    login an unique token will be generated
    according to the role and id.
-}
type LoginApi = "login"
            :> ReqBody '[JSON] Login
            :> Post    '[JSON](ApiResponse String)

{-| API endpoint toedit the product details for a given product id by providing
    product id as capture
    Token can be passed using as bearer token
-}
type EditProducts = "editproduct"
                  :> Auth    '[JWT] AccessData
                  :> ReqBody '[JSON] EditProduct
                  :> Post    '[JSON] (ApiResponse String)

{-| API endpoint to remove the product from inventory for a given
    product id by providing JWT authorization token.
    Token can be passed using as bearer token
-}
type RemoveProduct = "removeproduct"
                   :> Auth   '[JWT] AccessData
                   :> Capture "productid" Int
                   :> Delete '[JSON] (ApiResponse String)

type TempPriceChange = "temporarypricechange"
                     :> Auth    '[JWT]  AccessData
                     :> ReqBody '[JSON] AddTempValueInfo
                     :> Post    '[JSON] (ApiResponse String)

type AcceptOrRejectOrder = "orderstatus"
                :> Auth '[JWT] AccessData
                :> Capture "orderid" Int
                :> QueryParam "statusKey" AcceptOrReject
                :> Post    '[JSON] (ApiResponse String)

type CartQuantityUpdate = "cartquantityupdate"
                :> Auth    '[JWT] AccessData
                :> Capture "productid" Int
                :> QueryParam "quantity" Int
                :> Post    '[JSON] (ApiResponse String)

{-| API endpoint to view the wish listed products for a by providing JWT
     authorization token.
     Token can be passed using as bearer token
-}
type ViewWishlist = "viewwishlist"
                  :> Auth '[JWT]   AccessData
                  :> Get  '[JSON] (ApiResponse [WishList])