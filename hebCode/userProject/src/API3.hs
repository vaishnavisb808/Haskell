{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs#-}

{-|
Module          : API
Description     : Functions to create API Structure, Initializing Connection using Connection Pool and defining data types
-}

module API3 where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import Data.Pool
import Data.Aeson.TH
import GHC.Generics

import qualified Data.Text as T
import Control.Monad
import GHC.Generics
import Database.PostgreSQL.Simple
import Data.Map
import qualified Data.Map as Map
import Control.Monad.IO.Class

import Database.PostgreSQL.Simple
import Data.Map
import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Monad
import qualified Data.Text as T
import System.Directory.Internal.Prelude (lookupEnv)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import System.Environment
import Data.ByteString.Lazy.Char8 as BSL8
import GHC.Int
import Config
import Control.Monad.Trans.Reader
import Logger
import LoggerTypes
import DbOps
import CoreTypes

import qualified Data.Map as Map
import Data.Map
import Data.Pool (withResource)
import qualified Data.Text as T
import Servant.Swagger
import Data.Swagger 
import Control.Monad (Monad(return))
import OriginalImageHandler2
import ObjectDetection
import Label
import qualified Data.ByteString.Char8 as Bytechar




-- | request body model for image upload
-- newtype InputData = InputData{
--     imagePayLoad :: String  -- ^ url to be given as the request body to the post method
-- }deriving(Generic)
 
 
-- -- | data type for each object detected in the image 
-- instance ToJSON InputData
-- instance FromJSON InputData
-- instance ToSchema InputData


-- | The API for serving @swagger.json
type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger
          :<|> "docs":> Raw


-- | defining the type of API which has the following endpoints
-- | API type signature
type VisionApi = "images" :> QueryParam "objects" String :> Get '[JSON] [ImageDetails]
          :<|> "images" :> Capture "imageId" Int :> Get '[JSON] ImageDetails
          :<|> "images" :> ReqBody '[JSON] InputData :> Post '[JSON] ImageDetails
         
--  | combining both API
type API =  VisionApi :<|> SwaggerApi
          


type ApiHandler = ReaderT Env Handler 

type Database = PostgresDb

-- | Globally shared values
data Env where
    Env::DbOps a => {
          envConfig::Configuration
        , envDb:: a 
        , envLogger:: LogLevel -> String -> IO()
    }-> Env

-- | API handlers 
server :: ServerT API ApiHandler 
server =    (getImages :<|> getImage :<|> imageHandler)
        :<|>( getSwagger :<|> serveDirectoryWebApp "static")
     where 
          -- getImages is a handler for theget request to /images
          -- it returns all the metadata of images if the query parameter is not present
          -- else it returns the images that contains the object specified in the query parameter
           getImages:: Maybe String -> ApiHandler [ImageDetails]
           getImages object  = do
                case object of
                 --  get images that have a given list of objects in them
                 (Just a) -> do          
                        --split the string of objects into list of objects  
                        let splitData = T.splitOn (T.pack ",") (T.pack a)  
                        env <- ask 
                        case env of 
                          Env{envDb=db}->do
                            images <- liftIO$ getImagesByTag (db) splitData
                            case images of
                                Right images -> return images
                                Left msg -> do
                                  log ERROR "sql error"
                                  throwError err503{errBody="Failed to fetch Db"}
                 -- get all image metadata
                 Nothing -> do
                        env<-ask
                        case env of
                          Env{envDb=db} -> do
                              images <- liftIO$ getAllImages (db)
                              case images of
                                Right images -> return images
                                Left msg -> do
                                  log ERROR "sql error"
                                  throwError err503{errBody="Failed to fetch Db"}
           
           -- getSpecificImage is a handler for the get request to /images{imageId}   
           -- it returns the metadata of that specific image                                 
           getImage :: Int -> ApiHandler ImageDetails  
           getImage imageId = do
               env <- ask
               case env of 
                 Env{envDb=db} -> do
                     image <- liftIO$ getImageById db imageId
                     case image of 
                      Right image -> return image  
                      Left msg -> do
                        log ERROR "sql error"
                        throwError err503 {errBody = "failed to fetch db"}

           -- uploadImage is a handler for the post request to /images        
           imageHandler :: InputData -> ApiHandler ImageDetails
           imageHandler imageUrl = do
                   let imgbase64 = imagebase64 imageUrl --extracting image url
                   let label = image_label imageUrl -- extracting label
                   let byteImage = Bytechar.pack imgbase64 -- converting image url to bytestring
                   let auth = header "Authorization" "Basic YWNjXzQ2YzdjOTU0YTNiZGM0YjphNmY4NmU3NWQwMDcyMmUxNjMxNTI5NmU2MmJhODdiOA==" -- Imagga authentication details
                  
                   uploadId <- (upload byteImage)  auth --calling uploadImage function

                   tagOut <- objectDetectUrl auth uploadId
                   env <- ask
                   case env of 
                      Env{envDb=db} -> do
                        imgIdLabel <- liftIO $  insertImageToDb db uploadId label--tagOut
                   -- withResource conns $ \conn ->
                        case imgIdLabel of
                                Right imgIdLabel -> return imgIdLabel
                                Left msg -> do
                                  log ERROR "Failed to insert label"
                                  throwError err503{errBody="Failed to insert label"}

           
                        -- let image_id = fst $ Prelude.head imgIdLabel
                        -- let imageLabel = snd $ Prelude.head imgIdLabel

                    -- serverResponse <- liftIO $  finalResponse db  imageUrl image_id imageLabel uploadId auth tagOut --calling userResult function
                    -- return  serverResponse

           -- helper function to log message
           log::LogLevel -> String -> ApiHandler ()
           log logLevel message = do
               env<-ask
               liftIO $ envLogger env logLevel message

           getSwagger:: ApiHandler Swagger
           getSwagger= return $(toSwagger visionApi) 

-- | Creating new connection pool
initConnectionPool ::IO Connection -> IO (Pool Connection)
initConnectionPool connectionInitialize = do
  createPool connectionInitialize
             close
             1 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe


-- | Initializing database connection by using credentials from environment variable.
initializeConnection :: Configuration -> IO Connection
initializeConnection configVar = do
    connect defaultConnectInfo{
      connectDatabase= dbName configVar,
      connectUser = dbUser configVar,
      connectPassword = dbPass configVar,
      connectHost = dbHost configVar
    }  


api :: Proxy API 
api = Proxy

visionApi :: Proxy VisionApi
visionApi = Proxy


nt :: Env -> ApiHandler a -> Handler a
nt env apiHandler = runReaderT apiHandler env

app :: Env -> Application 
app env= serve api $ hoistServer api (nt env) server  

startApp :: IO()
startApp = do
    config <- getConfig
    case config of
        Right config -> do
           let logger = runLog (getLogger config)
           logger INFO "Connecting to DB ..."
           let conn =  initializeConnection config
           connPool <- initConnectionPool conn
           withResource connPool (\conn -> initializeSchema conn)
           logger INFO ("Server running on Port" ++ show portNumber)
           let postgresDb = PostgresDb connPool
           run portNumber (app (Env config  postgresDb logger))
               where portNumber = serverPort config 
        Left errMsg -> print errMsg

-- | Script to initialize database
initializeSchema :: Connection -> IO Int64
initializeSchema con = do
    execute_ con "CREATE TABLE IF NOT EXISTS image_details \
                 \(image_id Int not null , image_label varchar(512) not null, \
                 \PRIMARY KEY(image_id));"
        
    execute_ con "CREATE TABLE IF NOT EXISTS image_tags \
                 \(image_id int NOT NULL,confidence Float NOT NULL,\
                    \tag varchar(512),\
                 \FOREIGN KEY (image_id) REFERENCES image_details(image_id));"
        
