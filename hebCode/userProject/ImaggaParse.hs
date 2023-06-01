{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
--module ImaggaParse where
import qualified Data.Text as T
import  Data.Aeson 
import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import qualified Data.ByteString.Lazy as B
import GHC.Int
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow (ToRow(toRow))
-- import GHC.Runtime.Eval (Term(val))

data ImageDetails''' = ImageDetails''' {
   result :: ImageDetails''
      } deriving(Generic,Show)

data ImageDetails'' = ImageDetails''{
    tags :: [ImageDetails']
    } deriving(Generic,Show)

data ImageDetails' = ImageDetails'{
    confidence' :: Double,
    tag' :: InputData
    } deriving(Generic,Show)    

data InputData= InputData {
    en::String}
    deriving(Generic,Show)

-- | data type for each object in the image
data ImageTag = ImageTag{
    confidence :: Double,
     tag :: String
} deriving(Generic,Show)
-- | data type of an image
data ImageDetails = ImageDetails{
    imageId::Int,
    label :: String,
    imagetag :: [ImageTag]
} deriving(Generic,Show)

instance FromJSON ImageDetails'''
instance FromJSON ImageDetails''
instance FromJSON ImageDetails'
instance FromJSON InputData

instance ToRow ImageDetails where
    toRow imgDetails = [toField(imageId imgDetails), toField(label imgDetails)] 

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x
mains = do
    let img_label="fine.png"::String
    conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
    rows <-execute conn "insert into image_details (image_label) values (?)" (Only img_label)
    print rows
    inp <-B.readFile "imageres.json"
    let y = decode inp::Maybe Value
    let val = fromJust y
    print val
    
    
    