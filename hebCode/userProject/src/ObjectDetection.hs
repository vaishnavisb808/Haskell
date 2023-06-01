{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module ObjectDetection where

import ObjDetectionTypeClass
import Data.Aeson
import Network.HTTP.Req 
import GHC.Generics
import GHC.Int
import Data.ByteString as B
import qualified Network.HTTP.Client.MultipartFormData as LM
import Database.PostgreSQL.Simple
import Data.ByteString.Lazy as BS
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as Bytechar
import Servant(Handler)
import CoreTypes

--return type of /upload (post method)endpoint of imagga
data Image = Image{
    result1 :: Upload
} deriving (Generic,Show)

data Upload = Upload {
    upload_id :: String
} deriving (Generic,Show)


instance FromJSON Upload
instance FromJSON Image where
   parseJSON = withObject "Image" $ \v -> Image
        <$> v .: "result"

-- -- return type of /tags endpoint of imagga
data ImageD = ImageD {
   result :: ImageDe
      } deriving(Generic,Show)

instance FromJSON ImageD

data ImageDe = ImageDe{
    tags :: [ImgTag]
   } deriving(Generic,Show)

instance FromJSON ImageDe


data ImgTag = ImgTag{
    confidence :: Double,  -- ^ Confidence score of detected object
    tag :: ImageLabel     -- ^ Name of the detected object in the given image
} deriving(Generic,Show,Eq)
instance FromJSON ImgTag

data ImageLabel = ImageLabel {
    en ::String
    } deriving(Generic,Show,Eq) 
instance FromJSON ImageLabel

upload url autht = do
    body <-
        reqBodyMultipart
         [ LM.partBS "image_base64" url
          ]
    imaggapost <- runReq defaultHttpConfig $ do
                                    r <-
                                        req
                                        POST
                                        (https "api.imagga.com"/:"v2"/:"uploads")
                                        body
                                        jsonResponse 
                                        autht
                                    return (responseBody r::Image) 
    let upld = upload_id $ result1 imaggapost
    return upld


-- objectDetectFile autht url = do
--      body <-
--            reqBodyMultipart
--              [ LM.partBS "image_base64" url
--                    ]
--      result2 <- runReq defaultHttpConfig $ do
                                   
--                                     r <-
--                                         req
--                                         POST
--                                         (https "api.imagga.com"/:"v2"/:"tags")
--                                         body
--                                         jsonResponse 
--                                         autht
--                                     return (responseBody r::ImageD)
--      let taglist = tags $result result2
--      let out = recurseList taglist
--      return out

     


objectDetectUrl autht img_uploadid = do
    
    result2 <- runReq defaultHttpConfig $ do
                                    let qparam =
                                         "image_upload_id" =: img_uploadid <>
                                         autht
                                    r <-
                                        req
                                        GET
                                        (https "api.imagga.com"/:"v2"/:"tags")
                                        NoReqBody
                                        jsonResponse 
                                        qparam
                                    return (responseBody r::ImageD)
    let taglist = tags $result result2
    let out = recurseList taglist
    return out

recurseList [] = []
recurseList taglist  = do
        let conf = ObjectDetection.confidence $ Prelude.head taglist
        let en1 = en $ ObjectDetection.tag $ Prelude.head taglist 

        let img = [ImageTag conf en1]
        img++recurseList (Prelude.tail taglist)