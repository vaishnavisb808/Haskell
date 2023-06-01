{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module VisionApi.Types(ImageTag(..),ImageDetails(..),ApiResponse(..),apiSuccessResponse) where

import           Data.Aeson
import           Data.Swagger
import           GHC.Generics
import           Lens.Micro
import           Servant


-- | data type for each object detected in the image
data ImageTag = ImageTag
    { confidence :: Double    -- ^ Confidence score of detected object
    , tag        :: String        -- ^ Name of the detected object in the given image
    } deriving(Generic,Eq,Show)

-- | data type of an image
data ImageDetails = ImageDetails
   { imageId   :: Int  -- ^ Unique id for each image in the database
   , label     :: String   -- ^ Image label given by user
   , imagetags :: [ImageTag]
   } deriving(Generic,Eq,Show)

-- | request body model for image upload
data InputData = InputData
   { imagePayLoad     :: String   -- ^ url to be given as the request body to the post method
   , image_label      :: String
   , object_detection :: String
   } deriving(Generic)

-- | data type for each object detected in the image
data ObjectDetails = ObjectDetails
   { objectId    :: Int  -- ^ Unique id for each image in the database
   , objectLabel :: String  -- ^ Name of the image which will considered as the label if the API does not detect any object
   }deriving(Generic,Show)

data ApiResponse a = ApiResponse
    { result :: a
    , error  :: Maybe Int
    , code   :: Int
    }deriving (Show,Generic)

instance (ToJSON a)=>ToJSON (ApiResponse a)
instance (ToSchema a)=>ToSchema (ApiResponse a)

apiSuccessResponse::(ToJSON a, FromJSON a)=> a->ApiResponse a
apiSuccessResponse body = ApiResponse body Nothing 200

instance ToJSON InputData
instance ToJSON ImageDetails
instance ToJSON ImageTag
instance ToJSON ObjectDetails

instance ToSchema ImageDetails
instance ToSchema ImageTag

instance FromJSON InputData
instance FromJSON ImageDetails
instance FromJSON ImageTag
