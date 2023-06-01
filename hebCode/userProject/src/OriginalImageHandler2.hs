{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module OriginalImageHandler2 where


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
import Label
import CoreTypes
import ObjectDetection
import DbOps

-- | data type for the request body of the endpoint with Post method
-- data InputData = InputData{
--     imagebase64 :: String,
--     image_label :: String,
--     object_detection :: String  
-- }deriving(Generic,Show)

-- instance FromJSON InputData


--return type of /upload (post method)endpoint of imagga
-- data Image = Image {
--     result1 :: Upload
-- } deriving (Generic,Show)

-- data Upload = Upload {
--     upload_id :: String
-- } deriving (Generic,Show)

-- instance FromJSON Upload
-- instance FromJSON Image where
--    parseJSON = withObject "Image" $ \v -> Image
--         <$> v .: "result"

-- return type of /tags endpoint of imagga
-- data ImageD = ImageD {
--    result :: ImageDe
--       } deriving(Generic,Show)

-- instance FromJSON ImageD

-- data ImageDe = ImageDe{
--     tags :: [ImgTag]
--    } deriving(Generic,Show)

-- instance FromJSON ImageDe

-- data ImgTag = ImgTag{
--     confidence :: Double,  -- ^ Confidence score of detected object
--     tag :: ImageLabel     -- ^ Name of the detected object in the given image
-- } deriving(Generic,Show,Eq)
--instance FromJSON ImgTag where

-- data ImageLabel = ImageLabel {
--     en::String
--     } deriving(Generic,Show,Eq)
--instance FromJSON ImageLabel   

-- | data type of an image
-- data ImageDetails = ImageDetails{
--     imageId::Int,
--     label :: String,
--     imagetags :: [ImageTag]
-- } deriving(Generic,Show)

-- | data type for each object in the image
-- data ImageTag = ImageTag{
--     confidance :: Double,
--     tag1 :: String
-- } deriving(Generic,Show)

-- instance ToRow ImageTag

-- instance FromJSON ImgTag
-- instance FromJSON ImageLabel

-- | function to send request to /uploads endpoint of Imagga
-- uploadImage url autht = do
--     body <-
--         reqBodyMultipart
--          [ LM.partBS "image_base64" url
--           ]
--     imaggapost <- runReq defaultHttpConfig $ do
--                                     r <-
--                                         req
--                                         POST
--                                         (https "api.imagga.com"/:"v2"/:"uploads")
--                                         body
--                                         jsonResponse 
--                                         autht
--                                     return (responseBody r::Image) 
--     let uploadid = upload_id $ result1 imaggapost
--     return uploadid

-- --function to send request to /tags endpoint of Imagga
-- imageTagging img_uploadid autht = do
--      result2 <- runReq defaultHttpConfig $ do
--                                     let qparam =
--                                          "image_upload_id" =: img_uploadid <>
--                                          autht
--                                     r <-
--                                         req
--                                         GET
--                                         (https "api.imagga.com"/:"v2"/:"tags")
--                                         NoReqBody
--                                         jsonResponse 
--                                         qparam
--                                     return (responseBody r::ImageD)
--      let taglist = tags $result result2
--      let out = recurseList taglist
--      return out

-- function for server response and inserting image tag to DB
finalResponse conn userInput uimageid  ilabel uploadId auth tagOut =do
    let detection = object_detection userInput
    if (detection == True)
         then do
                     -- calling imageTagging function
                    --let out = recurseList tagOut  -- gets a list of imageTag type
                    let rows = Prelude.map  (\imageTag-> (uimageid,CoreTypes.confidence imageTag, tag1 imageTag)) tagOut
                    
                    executeMany conn
                         "insert into public.imageapi (imageid,confidence,obj) values (?,?,?)" rows
                    
                    let serverResp = ImageDetails uimageid ilabel tagOut
                    return serverResp             
    else do
        let serverRespNoDetection = ImageDetails uimageid ilabel [ImageTag 0 ""]
        return serverRespNoDetection

--function to get a list of ImageTag type
-- recurseList [] = []
-- recurseList taglist  = do
--         let conf = OriginalImageHandler2.confidence $ Prelude.head taglist
--         let en1 = en $ tag $ Prelude.head taglist 

--         let img = [ImageTag conf en1]
--         img++recurseList (Prelude.tail taglist) 
         
insertLabel label conn uploadId tagOut= 
    if not(Prelude.null label)
         then do
            insertimage <- insertImageToDb conn uploadId label
            return insertimage
            -- xs <- query conn"insert into public.image_details (upload_id,image_label) \
            --          \values (?,?) returning imageid ,image_label" (uploadId::String,label::String) :: IO [(Int,String)]
            -- let imageid = fromOnly $ Prelude.head xs
            -- return xs 
         else do
             functLabel<-generateUnique uploadId tagOut
             insertimage <- insertImageToDb conn uploadId functLabel
             return insertimage
            --  xs <- query conn"insert into public.image_details (upload_id,image_label) \
            --          \values (?,?) returning imageid,image_label" (uploadId::String,functLabel::String) :: IO [(Int,String)] 
            --  let imageid = fromOnly $ Prelude.head xs
            --  return xs 


-- import Text.Regex.Posix ((=~))




-- isUrlOrFile :: String -> Bool

-- isUrlOrFile (urlOrFile ) = urlOrFile =~ "(https://|http://)[a-zA-Z./_0-9-]*"