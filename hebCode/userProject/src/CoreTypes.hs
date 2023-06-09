{-# LANGUAGE DeriveGeneric #-}

module CoreTypes(ImageTag(..),ImageDetails(..),InputData(..)) where

import Data.Aeson
import GHC.Generics
--import Data.Swagger
import Database.PostgreSQL.Simple
-- | data type for each object detected in the image 
data ImageTag = ImageTag{
    confidence :: Double,  -- ^ Confidence score of detected object
    tag1 :: String      -- ^ Name of the detected object in the given image

} deriving(Generic,Eq)

-- | data type of an image
data ImageDetails = ImageDetails{
    imageId::Int,  -- ^ Unique id for each image in the database
    label :: String,  -- ^ Image label given by user, label is autogenerated when user doesn't provide one
    imagetags :: [ImageTag] 
} deriving(Generic,Eq)

-- | request body model for image upload
-- newtype InputData = InputData{
--     imagePayLoad :: String  -- ^ url to be given as the request body to the post method
-- }deriving(Generic)
 
data InputData = InputData{
    imagebase64 :: String,
    image_label :: String,
    object_detection :: Bool   
}deriving(Generic,Show)

-- | data type for each object detected in the image 
data ObjectDetails = ObjectDetails
 {
   objectId::Int,  -- ^ Unique id for each image in the database
   objectLabel :: String  -- ^ Name of the image which will considered as the label if the API does not detect any object 
 }deriving(Generic,Show)

instance FromJSON InputData

instance ToJSON InputData
instance ToJSON ImageDetails 
instance ToJSON ImageTag
instance ToJSON ObjectDetails

-- instance ToSchema ImageDetails
-- instance ToSchema ImageTag
-- instance ToSchema InputData

-- instance FromJSON InputData
instance FromJSON ImageDetails
instance FromJSON ImageTag
instance ToRow ImageTag

