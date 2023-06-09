{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Imaggarequest where


import Data.Aeson
import Network.HTTP.Req 
import GHC.Generics
import Data.ByteString as B
import qualified Network.HTTP.Client.MultipartFormData as LM
import Database.PostgreSQL.Simple
import Data.ByteString.Lazy as BS

data ImageTag = ImageTag{
    confidence :: Double,  -- ^ Confidence score of detected object
    tag :: String      -- ^ Name of the detected object in the given image

} deriving(Generic)

-- | data type of an image
data ImageDetails = ImageDetails{
    imageId::Int,  -- ^ Unique id for each image in the database
    label :: String,  -- ^ Image label given by user, label is autogenerated when user doesn't provide one
    imagetags :: [ImageTag] 
} deriving(Generic)

-- | data type for the request body of the endpoint with Post method
data InputData = InputData{
    url :: String,
    label1 :: String,
    enable :: String  -- ^ url to be given as the request body to the post method
}deriving(Generic,Show)

instance FromJSON InputData


data Image = Image {
    result :: Upload,
    status :: Status
} deriving (Generic,Show)


data Upload = Upload {
    upload_id :: String
} deriving (Generic,Show)


data Status = Status {
    text :: String,
    types :: String
} deriving (Generic,Show)

instance FromJSON Image

instance FromJSON Upload

instance FromJSON Status where
    parseJSON = withObject "Status" $ \v -> Status
        <$> v .: "text"
        <*> v .: "type"


fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

mains :: IO()
mains = do
    inp <-BS.readFile "input.json"
    let y = decode inp::Maybe InputData
    let val = fromJust y
    -- let filename = url val 
    -- filename
    -- return val
    upload <- uploadImage val
    print upload

-- uploadImage :: InputData -> Handler ImageDetails





-- uploadImage :: InputData -> IO Image
uploadImage imageurl = do
        let file = url imageurl
        let label = label1 imageurl
        filecontent <- B.readFile file
        let auth = header "Authorization" "Basic YWNjXzQ2YzdjOTU0YTNiZGM0YjphNmY4NmU3NWQwMDcyMmUxNjMxNTI5NmU2MmJhODdiOA=="
        body <-
         reqBodyMultipart
         [ LM.partBS "image_base64" filecontent
          ]
        result1 <- runReq defaultHttpConfig $ do
                                    r <-
                                        req
                                        POST
                                        (https "api.imagga.com"/:"v2"/:"uploads")
                                        body-- use built-in options or add your own
                                        jsonResponse -- specify how to interpret response
                                        auth
                                    return (responseBody r::Image)
        let uploadid = upload_id $ result result1
        conn<- initializeConnection
        xs <- execute conn
                    "insert into public.image_details (upload_id,image_label) values (?,?)" (uploadid::String,label::String) 
        print xs
        return result1
        result2 <- runReq defaultHttpConfig $ do
                                    let qparam = 
                                         "image_upload_id" =: uploadid <>
                                         auth
                                    r <-
                                        req
                                        GET
                                        (https "api.imagga.com"/:"v2"/:"tags")
                                        NoReqBody-- use built-in options or add your own
                                        jsonResponse -- specify how to interpret response
                                        qparam 
                                       
                                    return (responseBody r::Image)
        return result2
        --print xs

initializeConnection :: IO Connection
initializeConnection = do
  connect defaultConnectInfo {
    connectDatabase = "postgres",
    connectUser = "postgres",
    connectPassword = "Minnus"
  }