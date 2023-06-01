{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module VisionApi.ObjectDetection.Types where

import Data.Aeson
import GHC.Generics
import VisionApi.Types

--return type of /upload (post method)endpoint of imagga
data Image = Image
    { result1      :: Upload 
    , uploadStatus :: Status
    } deriving (Generic,Show)

data Status = Status
    { statusType :: String
    , text       :: String
    } deriving (Show,Generic)

instance FromJSON Status where
    parseJSON = withObject "Status" $ \v -> Status
        <$>v .: "type"
        <*>v .: "text"
instance ToJSON Status

data Upload = Upload {
    upload_id :: String
} deriving (Generic,Show)

instance FromJSON Upload
instance FromJSON Image where
   parseJSON = withObject "Image" $ \v -> Image
        <$> v .: "result"
        <*> v .: "status"

-- -- return type of /tags endpoint of imagga
data ImageD = ImageD {
   result                    :: ImageDe
                     ,status::Status
      } deriving(Generic,Show)
instance FromJSON ImageD


data ImageDe = ImageDe{
    tags :: [ImgTag]
   } deriving(Generic,Show)

instance FromJSON ImageDe

data ImgTag = ImgTag{
    confidence :: Double,  -- ^ Confidence score of detected object
    tag        :: ImageLabel     -- ^ Name of the detected object in the given image
} deriving(Generic,Show,Eq)
instance FromJSON ImgTag

data ImageLabel = ImageLabel {
    en ::String
    } deriving(Generic,Show,Eq)
instance FromJSON ImageLabel


