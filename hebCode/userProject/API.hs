{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module          : API
Description     : Functions to create API Structure, Initializing Connection using Connection Pool and defining data types
-}

module API where

import OriginalImageHandler    
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.Pool
import Data.Aeson.TH
import Network.HTTP.Req
import GHC.Generics
import Control.Monad.IO.Class
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import System.Environment
import Data.ByteString.Lazy.Char8 as BSL8
import GHC.Int
import qualified Data.ByteString.Char8 as Bytechar
import Label
import ObjectDetection

-- name of the env var to look for if fetching from file fails
configVarName = "CONFIG"
-- | config information required
data Configuration = Configuration
                  {
                    dbName::String,
                    dbUser::String,
                    dbPass::String
                    }deriving (Generic,Show)

instance FromJSON Configuration

--instance ToJSON InputData
instance ToJSON ImageDetails 
instance ToJSON ImageTag

--instance FromJSON InputData
instance FromJSON ImageDetails
instance FromJSON ImageTag

-- | defining the type of API which has the following endpoints
-- | API type signature
type API =  "images" :> Servant.QueryParam "objects" String :> Get '[JSON] [ImageDetails]
          :<|> "images" :> Capture "imageId" Int :> Get '[JSON] ImageDetails
          :<|> "images" :> ReqBody '[JSON] InputData :> Post '[JSON] ImageDetails

-- | API handlers 
server :: Pool Connection -> Server API
server conns  = getImages  :<|> getSpecificImage :<|> imageHandler 
         where 
              -- getImages is a handler for theget request to /images
              -- it returns all the metadata of images if the query parameter is not present
              -- else it returns the images that contains the object specified in the query parameter
               getImages:: Maybe String -> Handler [ImageDetails]
               getImages x = case x of 
                                (Just a)-> throwError err503 { errBody = "Missing query param" }
                                Nothing -> throwError err503 { errBody = "Sorry dear user." }  

               -- getSpecificImage is a handler for the get request to /images{imageId}   
               -- it returns the metadata of that specific image                                 
               getSpecificImage :: Int -> Handler ImageDetails  
               getSpecificImage id = do
                        message <- liftIO $ withResource conns $ \conn ->
                            query conn "SELECT image_details.image_id, image_label, \
                               \confidence, tag from image_details inner join image_tags on\
                                \ image_details.image_id=image_tags.image_id where image_details.image_id=?"[id]
                        let tags = fmap(\(_,_,confidence,tag)->ImageTag confidence tag)message
                        let details = (\(image_id,label,_,_)->ImageDetails image_id label tags)$ Prelude.head message
                        return (details)  

               -- uploadImage is a handler for the post request to /images        
               imageHandler :: InputData -> Handler ImageDetails
               imageHandler imageUrl = do
                   let imgbase64 = imagebase64 imageUrl --extracting image url
                   let label = image_label imageUrl -- extracting label
                   let byteImage = Bytechar.pack imgbase64 -- converting image url to bytestring
                   let auth = header "Authorization" "Basic YWNjXzQ2YzdjOTU0YTNiZGM0YjphNmY4NmU3NWQwMDcyMmUxNjMxNTI5NmU2MmJhODdiOA==" -- Imagga authentication details
                   uploadId <- (uploadImage byteImage) auth --calling uploadImage function
                   tagOut <- objectDetectUrl uploadId auth
                   image_id <- liftIO $ withResource conns $ \conn -> insertLabel label conn uploadId tagOut
                   serverResp <- liftIO $ withResource conns $ \conn -> finalResponse conn  imageUrl image_id uploadId auth label tagOut --calling userResult function
                   return  serverResp
                    
                    --throwError err503 { errBody = "Request cannot be completed at this moment" }

-- | Creating new connection pool
initConnectionPool ::IO Connection -> IO (Pool Connection)
initConnectionPool connectionInitialize = do
  createPool connectionInitialize
             close
             1 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

-- | read configuration string from envvar and parse the json string to interpret
--  it as Configuration type. returns a string error message on failure to find an
--  envvar with given name or failure to parse the contained json.
getConfigFromEnv::String->IO(Either String Configuration)
getConfigFromEnv envVarName = do
    configJsonString' <- lookupEnv envVarName
    case configJsonString' of
        Just configJsonString -> do
            let config' = decode (BSL8.pack configJsonString) :: Maybe Configuration
            case config' of 
                Just config -> return $Right config
                Nothing -> return $Left $"Unable to parse config obtained from env var : "++envVarName
        Nothing -> return $ Left $"Could not find env var"++envVarName 

-- | Initializing database connection by using credentials from environment variable.
initializeConnection :: Configuration -> IO Connection
initializeConnection configVar = do
    connect defaultConnectInfo{
      connectDatabase= dbName configVar,
      connectUser = dbUser configVar,
      connectPassword = dbPass configVar
 }  

api :: Proxy API
api = Proxy

app :: Pool Connection -> Application
app conn= serve api (server conn)        

startApp :: IO()
startApp = do
    config <- getConfigFromEnv configVarName
    case config of
        Right config -> do
           print "Connecting to DB ..."
           let conn =  API.initializeConnection config
           conn_pool <- initConnectionPool conn
           print ("Server running on Port" ++ show portNumber)
           run portNumber (app conn_pool)
           where portNumber = 8080
        Left config -> print "Env variable is yet to be set"
    print "Done"  

