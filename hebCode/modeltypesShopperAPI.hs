{-# LANGUAGE DuplicateRecordFields #-}
import Data.Time (UTCTime)

data ApiResponse a = ApiResponse
    { result :: a
    , error  :: Maybe Int
    , code   :: Int
    }

data User = User {
     email:: String
    ,password ::String
    ,userName :: String
    ,phoneNumber::Integer
    ,address :: Address 
}

data Address = Address {
     houseName :: String 
    ,streetName ::String
    ,landMark ::Maybe String
    ,city :: String 
    ,pincode :: Integer
    ,state ::String 
    ,country :: String
    ,phoneNumber :: Integer
}

data UserCredential =    Email {
                            email :: String } 
                        |UserName {
                            username ::String }
data Login = Login {
     userName :: UserCredential 
    ,password ::String
}

data WarehouseManger = WarehouseManger
 {
    email :: String
    ,password :: String
    ,phoneNumber ::String 
    ,wareHouseId :: Int 
}
data AddOrRemove = Add | Remove 



data AddOrUpdateProduct = AddOrUpdateProduct
 {
     productName :: String
    ,productDescription ::ProductDescription 
    ,category::ProductCategory 
    ,productPrice::Float 
    ,currency::String
    ,status ::String 
 }
data Quantity =  Kilo { 
                        kilo ::Float } 
                |Litre {
                         litre ::Float } 
                |Piece {
                         piece ::Int }

data ProductDescription = ProductDescription
 { 
     quantity :: Quantity
    ,description :: String
}

newtype ProductCategory = ProductCategory{
    productCategory :: String
}

data TemporaryPrice = TemporaryPrice {
    temporaryCurrency :: String 
  , temporaryValue ::Float
  , productId ::Int
  , effectiveStartDate :: UTCTime
  , effectiveEndDate ::UTCTime
}

data ProductStatus = ProductStatus
 {
      productId ::Int
     ,productStatus ::String
 }

data ProductStock = ProductStock
 {
      productId ::Int 
     ,stockCount :: Int 
     ,lastUpdate ::String 
 }

data ProductCart = ProductCart
 {
     productName ::String 
    ,quantity :: Quantity 
 }

data Order = Order
 {
      product :: [ProductCart]
     ,address ::Address
 }

data Tracking = Tracking
 {
      orderId ::Int 
     ,shipped ::Shipped
 }

data Shipped = Shipped
 {
     location :: String
    ,date ::UTCTime
}

data AcceptOrReject = Accept | Reject

data OrderAccept = OrderAccept{
     orderId ::Int 
    ,status ::AcceptOrReject
}

newtype WishList = WishList 
 {   productName ::String 
 }
data Rating = Rating {
     rating::Maybe Float 
    ,comment ::Maybe String
    ,productName :: String
}

data ProductDetails = ProductDetails {
     productName :: String
    ,productId ::Int
    ,productDescription ::ProductDescription 
    ,category::ProductCategory 
    ,productPrice::Float 
    ,currency::String
    ,temporaryPrice :: Maybe Float 
    ,numberOfReviews ::Int
    ,rating ::Float
    ,status ::String 
 }
newtype ViewProduct=ViewProduct
 {
     products ::[ProductDetails]
 }
data OrderDetails = OrderDetails
 {
      orderId ::Int 
     ,products :: [ProductCart]
     ,address ::Address 
     ,totalPrice ::Price
 }
newtype ViewOrder=ViewOrder
 {
     orders ::[OrderDetails]
 }
data ViewCart = ViewCart
 {
     cartId ::Int 
     ,products :: [ProductCart] 
     ,price ::Price
 }

data Price = Price{
     value ::Float
    ,currency::String
}

newtype ViewAddress= ViewAddress
 {addresses :: [Address]}