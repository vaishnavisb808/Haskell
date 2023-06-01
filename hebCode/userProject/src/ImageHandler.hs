{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module ImageHandler where

import Data.Aeson
import Network.HTTP.Req 
import GHC.Generics
import Data.ByteString as B
import qualified Network.HTTP.Client.MultipartFormData as LM
import Database.PostgreSQL.Simple
import Data.ByteString.Lazy as BS
import Control.Monad
import qualified Data.ByteString.Char8 as Bytechar


-- | data type for the request body of the endpoint with Post method
data InputData = InputData{
    url :: String,
    label1 :: String,
    enable :: String  
}deriving(Generic,Show)

instance FromJSON InputData
instance ToJSON InputData


--return type of /upload (post method)endpoint of imagga
data Image = Image {
    result1 :: Upload
} deriving (Generic,Show)

data Upload = Upload {
    upload_id :: String
} deriving (Generic,Show)

instance FromJSON Upload
instance FromJSON Image where
   parseJSON = withObject "Image" $ \v -> Image
        <$> v .: "result"

-- return type of /tags endpoint of imagga
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
instance FromJSON ImgTag where

data ImageLabel = ImageLabel {
    en::String
    } deriving(Generic,Show,Eq)
instance FromJSON ImageLabel   

-- | data type of an image
data ImageDetails = ImageDetails{
    imageId::Int,
    label :: String,
    imagetags :: [ImageTag]
} deriving(Generic,Show)

-- | data type for each object in the image
data ImageTag = ImageTag{
    confidance :: Double,
    tag1 :: String
} deriving(Generic,Show)

instance ToRow ImageTag



-- function to remove Just from maybe type
fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

-- function for database connection
initializeConnection :: IO Connection
initializeConnection = do
  connect defaultConnectInfo {
    connectDatabase = "postgres",
    connectUser = "postgres",
    connectPassword = "Minnus"
  }

--Handler function for '/images' api POST method
imageHandler  = do
    useripjson <-BS.readFile "input.json" --reading userinput as a json file
    let decodedinp = decode useripjson::Maybe InputData 
    let withoutjustinp = fromJust decodedinp --removing just
    let imageurl = url withoutjustinp --extracting image url
    let label = label1 withoutjustinp -- extracting label
    let byteImage = Bytechar.pack imageurl -- converting image url to bytestring
    let auth = header "Authorization" "Basic YWNjXzQ2YzdjOTU0YTNiZGM0YjphNmY4NmU3NWQwMDcyMmUxNjMxNTI5NmU2MmJhODdiOA==" -- Imagga authentication details
    uploadOut <- uploadImage byteImage auth --calling uploadImage function
    conn<- initializeConnection
    xs <- query conn
                    "insert into public.image_details (upload_id,image_label) \
                    \values (?,?) returning imageid" (uploadOut::String,label::String) :: IO [Only Int] --inserting uploadid and label to DB and returns imageid
    let imageid = fromOnly $ Prelude.head xs    
    serverResp <- userResult withoutjustinp imageid uploadOut auth label --calling userResult function
    return serverResp

-- | function to send request to /uploads endpoint of Imagga
uploadImage url autht = do
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
    let uploadid = upload_id $ result1 imaggapost
    return uploadid

--function to send request to /tags endpoint of Imagga
imageTagging imgupid autht = do
     result2 <- runReq defaultHttpConfig $ do
                                    let qparam =
                                         "image_upload_id" =: imgupid <>
                                         autht
                                    r <-
                                        req
                                        GET
                                        (https "api.imagga.com"/:"v2"/:"tags")
                                        NoReqBody
                                        jsonResponse 
                                        qparam
                                    return (responseBody r::ImageD)
     let con = tags $result result2
     return con

-- function for server response and inserting image tag to DB
userResult userInput uimageid uploadOut auth ilabel =do
    let detection= enable userInput
    conn<- initializeConnection
    if (detection=="True")
         then do
                    tagOut <- imageTagging uploadOut auth -- calling imageTagging function
                    let out = recurseList tagOut  -- gets a list of imageTag type
                    let rows = Prelude.map  (\imageTag-> (uimageid,confidance imageTag, tag1 imageTag)) out
                    --print rows
                    executeMany conn
                        "insert into public.imageapi (imageid,confidence,obj) values (?,?,?)" rows 
                    
                    let serRes = ImageDetails uimageid ilabel out
                    return serRes             
    else do
        let serResNoDet = ImageDetails uimageid ilabel [ImageTag 0 ""]
        return serResNoDet

--function to get a list of ImageTag type
recurseList [] = []
recurseList taglist  = do
        let conf = confidence $ Prelude.head taglist
        let en1 = en $ tag $ Prelude.head taglist 

        let img = [ImageTag conf en1]
        img++recurseList (Prelude.tail taglist) 
         

