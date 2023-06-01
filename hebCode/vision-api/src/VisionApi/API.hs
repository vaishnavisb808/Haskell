{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module          : API
Description     : Functions to create API Structure, Initializing Connection using
                  Connection Pool and defining data types
-}

module VisionApi.API where


import Control.Monad
import Control.Monad (Monad (return))
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Data.Pool (withResource)

import GHC.Int
import GHC.Generics
import Lens.Micro

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Servant.Swagger
import Data.Swagger

import VisionApi.Config
import VisionApi.Database.DbOps
import VisionApi.Database.PostgresDb
import VisionApi.Logger
import VisionApi.Logger.Types
import VisionApi.ObjectDetection.ImaggaService
import VisionApi.ObjectDetection.ObjectDetectionService
import VisionApi.Types
import VisionApi.Utils
import VisionApi.Logo

import Data.ByteString.UTF8 as BU8
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy as T
import Data.Map
import Data.Time
import Data.Aeson
import Data.Aeson.TH
import Data.Pool
import Text.Regex.Posix ((=~))

import System.IO
import System.Directory.Internal.Prelude (lookupEnv)
import System.Environment

import qualified Data.ByteString as B
import Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map as Map


-- | File upload identifier denoting whether image is being provided as base64 string or
--   Url
data FileOrUrl = File|Url deriving (Show,Eq,Generic)

instance FromJSON FileOrUrl
instance ToJSON FileOrUrl
instance ToSchema FileOrUrl

-- | request body model for image upload
data InputData = InputData{
      imagePayLoad      :: Text  -- ^ url to be given as the request body to the post method
    , imageLabel        :: Maybe String
    , imageObjectDetect :: Bool
    , imageType         :: FileOrUrl
}deriving(Generic)


instance ToJSON InputData
instance FromJSON InputData
instance ToSchema InputData




-- | The API for serving @swagger.json
type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger
          :<|> "docs":> Raw

-- | API Endpoint to get images containing specified Objects , returns all images if
--   objects query parameter is absent
type GetImagesByTag = Summary "Get images that contain specified objects, returns all \
                              \images if objects are not specified"
                      :> "images"
                      :> QueryParam "objects" String
                      :> Get '[JSON] (ApiResponse [ImageDetails])

-- | API Endpoint to get all metadata for a single image using it's imageId
type GetImageById = Summary "Get a single image using imageId"
                    :> "images"
                    :> Capture "imageId" Int
                    :> Get '[JSON] (ApiResponse ImageDetails)

-- | API Endpoint to upload an image as base64 string or provide a Url and perform
--   Object Detection on the image. The Object detection result and image metadata is
--   inserted into DB.
type UploadImage = Summary "Upload image and optionally perform object_detection"
                   :> "images"
                   :> ReqBody '[JSON] InputData
                   :> Post '[JSON] (ApiResponse ImageDetails)

-- | Core API Type
type VisionApi = GetImagesByTag
            :<|> GetImageById
            :<|> UploadImage

--  | Core API Type combined with Swagger Endpoints
type API =  VisionApi :<|> SwaggerApi


-- | Custom Monad used to wrap all handlers in a reader monad context so as to share
--   a few values as Env type across all handlers without explicitly passing them as
--   parameters
type ApiHandler = ReaderT Env Handler

-- | type alias for instance of DbOps
type Database = PostgresDb

-- | Globally shared values
data Env where
    Env::(DbOps a,ObjectDetectionService b) =>
        {     -- | Configuration info loaded on server boot
              envConfig  :: Configuration
              -- | Instance of DbOps shared across handlers
            , envDb      :: a
              -- | Instance of ObjectDetectionService shared across handlers
            , envObjDtct :: b
              -- | Logger function preloaded with configuration(LogLevel,LogType etc)
            , envLogger  :: LogLevel -> String -> IO()
        }-> Env




-- | API handlers
server :: ServerT API ApiHandler
server =    (getImages :<|> getImage :<|> uploadImage)
        :<|>( getSwagger :<|> serveDirectoryWebApp "static")
     where
          -- getImages is a handler for theget request to /images
          -- it returns all the metadata of images if the query parameter is not present
          -- else it returns the images that contains the object specified in the query parameter
           getImages:: Maybe String -> ApiHandler (ApiResponse [ImageDetails])
           getImages object  =withTimeLog $ do
                logInfo "recieved request -> /images?objects= GET"
                case object of
                  (Just str) -> do
                      -- split the string of objects into list of objects
                      let pattern = "^[a-zA-z][a-zA-Z,]*[a-zA-z]$" :: String
                      let isCommaSeparatedString = (str =~ pattern) :: Bool

                      if isCommaSeparatedString
                         then getImageByTag' str
                         else do
                          let logMessage =  "Invalid input:" ++ str
                                              ++ " Objects must be comma separated string"
                          logError logMessage
                          throwError $jsonError400 logMessage
                  -- get all image metadata
                  Nothing -> getAllImages'
             where

               getImageByTag' str=  do
                    -- split the comma separated string of objects into a list
                    let splitData = T.splitOn (T.pack ",") (T.pack str)
                    logDebug $"splitting string of objects into list of objects"
                                ++ show  splitData
                    let splitData' = Prelude.filter (/="") splitData

                    env <- ask
                    case env of
                      Env{envDb=db}->do
                        -- get images containing specified objects from DB
                        images <- liftIO$ getImagesByTag (db) splitData'

                        case images of
                            Right images' -> do
                              logDebug $"returning image metadata with specified objects "
                                        ++ show( Prelude.length images')
                              if  images' == [] then do
                                let noImage = ("No image found for tag : " ++ str)
                                logError noImage
                                throwError $jsonError404 noImage
                              else
                                return $apiSuccessResponse images'

                            Left msg -> do
                              logError msg
                              throwError $jsonError500 "Server Error : Unable to\
                                      \ fetch images with given object. Try again"
               getAllImages'  = do
                    logInfo "recieved request -> /images GET"
                    env<-ask
                    case env of
                      Env{envDb=db} -> do
                          logDebug "returning all image metadata"
                          images <- liftIO$ getAllImages db
                          case images of
                            Right images -> do
                              if images == []
                                then do
                                  logError "No record found in DB"
                                  throwError $jsonError404 "No data found. Try again"
                                else return $apiSuccessResponse images
                            Left msg -> do
                              logError msg
                              throwError $jsonError500 "Server Error : Unable to \
                                                                 \fetch images. Try again"




           -- handler for the get request to /images{imageId}
           -- it returns the metadata of that specific image
           getImage :: Int -> ApiHandler (ApiResponse ImageDetails)
           getImage imageId =  withTimeLog $ do
               logInfo "recieved request -> /images/{imageId} GET"
               env <- ask
               case env of
                 Env{envDb=db} -> do
                     image <- liftIO$ getImageById db imageId
                     case image of
                      Right [image] -> do
                        logDebug $"returning image metadata of specific image id "
                                    ++ show imageId
                        return $apiSuccessResponse image
                      Right [] -> do
                        logDebug $"No image found for imageId:" ++ show imageId
                        throwError $jsonError404 $"No image found for imageId"
                                                                        ++ show imageId
                      Left msg -> do
                        logError msg
                        let errorMsg="Server Error : Unable to fetch image with imageId :"
                                                             ++show imageId++", Try again"
                        throwError $jsonError500 errorMsg


           -- handler for the post request to /images,
           uploadImage :: InputData -> ApiHandler (ApiResponse ImageDetails)
           uploadImage uploadReqBody = withTimeLog $ do
               logInfo "recieved request -> /images POST"
               -- validate request
               let img' = imagePayLoad uploadReqBody
               let imgLabel' = imageLabel uploadReqBody
               let imgObjDetect' = imageObjectDetect uploadReqBody

               env<-ask
               case env of
                 Env{envObjDtct=service,envDb=db,envLogger=logger} -> do
                       (uniqueId,objects) <- case (imageType uploadReqBody) of
                            VisionApi.API.File -> uploadFileAndObjectDetect service img'
                            Url                -> objectDetectWithUrl service img'

                       -- auto generate a label if one is not provided
                       let imgLabel = case imgLabel' of
                              (Just label') -> label'
                              Nothing       -> autoGenLabel uniqueId objects


                       if not(Prelude.null objects)
                           then do
                              inserted <- liftIO
                                    $ insertImgAndTagsToDb db uniqueId imgLabel objects
                              logDebug "Inserting images and tags into DB"
                              case inserted of
                                Left msg -> do
                                  logDebug msg
                                  throwError  $jsonError500  "tag insertion failed...try again.."
                                Right imageId' -> do
                                    return $apiSuccessResponse
                                           $ImageDetails imageId' imgLabel objects
                           else do
                              imageId <- liftIO$ insertImageToDb db uniqueId imgLabel
                              case imageId of
                                Left msg -> do
                                   logDebug msg
                                   throwError $jsonError500 "Server error"
                                Right imgId ->
                                  return $ apiSuccessResponse $ImageDetails imgId imgLabel []

                   where
                     uploadFileAndObjectDetect db img'= do
                        logDebug "imagePayLoad matches file upload"
                        validateBase64 img'
                        uploadResult<-liftIO$ upload service img'
                        uploadId <- case uploadResult of
                              Left msg -> do
                                  logError $"failed to upload image"++msg
                                  throwError $jsonError500 "Upload failed"
                              Right uId -> return uId
                        logDebug $"image uploaded with uploadID: "++uploadId
                        if imgObjDetect'==True
                            then do
                                logDebug "Image Detection Enabled"
                                tags <- liftIO$ objectDetectFile service (T.pack uploadId)
                                case tags of
                                  Left msg -> do
                                    logError msg
                                    let errorMsg=" Unable to fetch tags , Try again"
                                    throwError $jsonError500 errorMsg

                                  Right imgTags-> do
                                    logDebug $"Tagging completed with "
                                        ++ show (Prelude.length imgTags) ++ " tags"
                                    return (uploadId,imgTags)
                            else return (uploadId,[])

                     objectDetectWithUrl service img' = do
                        logDebug "imagePayLoad matches url"
                        if imgObjDetect'==True
                            then do
                                logDebug "Image Detection Enabled"
                                tags <- liftIO$ objectDetectUrl service img'
                                case tags of
                                  Left msg -> do
                                    logError msg
                                    let errorMsg=" Unable to fetch tags , Try again"
                                    throwError $jsonError500 errorMsg
                                  Right imgTags-> do
                                    logDebug $"Tagging completed with "
                                           ++ show (Prelude.length imgTags) ++ " tags"
                                    return (T.unpack img',imgTags)
                            else return (T.unpack img',[])


           -- helper function to log message
           log::LogLevel -> String -> ApiHandler ()
           log logLevel message = do
               env<-ask
               liftIO $ envLogger env logLevel message

           logError = log ERROR
           logDebug = log DEBUG
           logInfo  = log INFO

           {- Get swagger documentation for the API
               throws bad request error if accessed from environment other than DEV
               i.e requires execEnv=DEV
           -}
           getSwagger:: ApiHandler Swagger
           getSwagger= do
               config <- asks envConfig
               if execEnv config == DEV
                  then
                     return $(toSwagger visionApi)
                      & info.title        .~ "Vision API"
                      & info.version      .~ "1.0"
                      & info.description  ?~ apiDescription
                  else do
                      logError "Trying to acess swagger from non dev environment"
                      throwError $jsonError400 "swagger is accesible only in DEV environment"
             where
               apiDescription = "This is an HTTP REST API for a service that ingests \
                                \user images, analyzes them for object detection and \
                                \returns the enhanced content. There are three endpoints,\
                                \one is to get all images and also for taking only images\
                                \that have the detected objects specified in the query \
                                \parameter. The second endpoint is for getting image \
                                \details for a specific image. The last endpoint is for \
                                \sending JSON request body including an image file or \
                                \URL, an optional label for the image, and an optional \
                                \field to enable object detection. This endpoint returns\
                                \JSON response body including the image data, its label, \
                                \and its identifier provided by the persistent data \
                                \store, and any objects detected."

           autoGenLabel :: String->[ImageTag] -> [Char]
           autoGenLabel uniqueId details = do
                if not(Prelude.null details) then do
                    let high_confident_tags = Prelude.take 2 $ details
                    let len = Prelude.length high_confident_tags
                    let head_high_confident_tags = Prelude.head high_confident_tags
                    let first_tag =  tag head_high_confident_tags
                    if len == 1 then
                        (uniqueId ++ "_" ++ first_tag)
                    else do
                        let tail_high_confident_tags = Prelude.tail high_confident_tags
                        let second_tag_list = Prelude.head tail_high_confident_tags
                        let second_tag = tag second_tag_list
                        (uniqueId ++ "_" ++ first_tag ++ "&" ++ second_tag)
                else
                    uniqueId


           {- Execute an API Handler along with a measure of time taken to do so.
              The time taken is logged with INFO Level in seconds
           -}
           withTimeLog::ApiHandler a -> ApiHandler a
           withTimeLog action = do
                start <-liftIO$  getCurrentTime
                !result <- action
                stop <- liftIO$ getCurrentTime
                logInfo  $"Request completed in : "++show(diffUTCTime stop start)
                return result


           {- validate a base64 String for characters present as well as validate if the
              size of the string is less than the configured limit
           -}
           validateBase64::Text -> ApiHandler Text
           validateBase64 base64String = do
              config <- asks envConfig
              let bs64WithoutHeader = stripHeader base64String
              case bs64WithoutHeader of
                Just str -> do
                    if T.null str
                       then throwError $jsonError400 "base64 string cannot be empty"
                       else return ()
                    let sizeOfStr = sizeOfBase64 str
                    if isValidBase64 str
                        then do
                            if sizeOfStr > maxImageSize config
                                  then do
                                      logInfo $"Image size exceeds limit : "
                                                 ++ show (maxImageSize config)
                                                 ++ " Image size = "
                                                 ++ show sizeOfStr
                                      throwError $jsonError400
                                                 $"Image exceeds size limit of "
                                                    ++show (maxImageSize config)
                                  else do
                                      logDebug $"image size: "++ show sizeOfStr
                                      return base64String
                    else throwError $jsonError400 "Invalid base64 string"
                Nothing  -> throwError $jsonError400 "Invalid base64 string,should \
                                                      \contain header and encoded text"


           {- Helper function to produce json formatted response body along with
              appropriate status code for error messages
           -}
           body code msg= object ["code".=(code::Int),"error".=msg,"result".=Null]

           jsonError400::String->ServerError
           jsonError400  msg = err400{ errBody=encode (body 400 msg),
                                   errHeaders = [("Content-Type", "application/json")]
                                 }
           jsonError404::String->ServerError
           jsonError404  msg = err404{ errBody=encode (body 404 msg)
                                , errHeaders = [("Content-Type", "application/json")]
                                }
           jsonError500::String->ServerError
           jsonError500  msg = err500{ errBody=encode (body 500 msg)
                            , errHeaders = [("Content-Type", "application/json")]
                            }



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
app env= serveWithContext
                api
                (customFormatters Servant.:. EmptyContext)
                (hoistServer api (nt env) server)


customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { bodyParserErrorFormatter = customFormatter
  , urlParseErrorFormatter   = customFormatter
  , notFoundErrorFormatter   = notFoundFormatter
  }

customFormatter :: ErrorFormatter
customFormatter combntr req err = err400
    { errBody = encode body
    , errHeaders = [("Content-Type", "application/json")]
    }
  where
    body = object ["code".=(400::Int),"error".=err,"result".=Null]

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req = err404 { errBody = "Path not found" }

startApp :: IO()
startApp = do
    Prelude.putStrLn logo
    config <- getConfig
    hSetBuffering stdout NoBuffering
    case config of
        Right config -> do
           let logger = runLog (getLogger config)
           logger INFO "Connecting to DB ..."
           let conn =  initializeConnection config
           connPool <- initConnectionPool conn
           logger INFO ("Server running on Port" ++ show portNumber)
           let postgresDb = PostgresDb connPool
           db <- initializeSchema postgresDb
           case db of
             Left msg -> do
                 logger ERROR "Failed to initialise schema"
             Right _  -> do
                 let objectDtctService = BasicAuth $BU.fromString(authToken config)
                 run portNumber (app (Env config  postgresDb objectDtctService logger))
          where
            portNumber = serverPort config
        Left errMsg -> print errMsg

