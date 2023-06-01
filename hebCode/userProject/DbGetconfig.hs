{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module DbGetconfig where

import Config
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Client hiding (Proxy,responseBody)
import Network.HTTP.Client.MultipartFormData
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.IO.Class
import Servant.Swagger
import Data.Swagger
import System.Environment(getEnv, lookupEnv)
import Servant.Multipart
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Internal as Text
import Control.Monad.Trans.Maybe
import Network.HTTP.Req
import Config (Configuration(dbPassword))
import Database.PostgreSQL.Simple 



data User= User{
    userId::Int,
    userName::String,
    userAge::Int,
    userEmail::String,
    userImage :: Maybe String
} deriving (Generic)

instance FromJSON User 

instance ToJSON User     

instance FromRow User

instance ToRow User

instance ToSchema User


data UserDetails = UserDetails{
    name::String,
    age::Int,
    email::String
} deriving (Generic)

instance FromJSON UserDetails 

instance ToJSON UserDetails 

instance ToRow UserDetails

instance ToSchema UserDetails


data UploadResponse = UploadResponse {
    success::Bool,
    uploadedFiles::[UploadedFile]
}

instance FromJSON UploadResponse where
    parseJSON = withObject "UploadResponse" $ \v -> UploadResponse
        <$> v .: "success"
        <*> v .: "files"

data UploadedFile = UploadedFile{
    fileName::String,
    fileUrl::String
} deriving (Generic)

instance FromJSON UploadedFile where
    parseJSON = withObject "UploadedFile" $ \v -> UploadedFile
        <$> v .: "name"
        <*> v .: "url"

instance ToJSON UploadedFile

type API = "swagger.json" :> Get '[JSON] Swagger 
    :<|> "docs" :> Raw
    :<|> "upload" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[JSON] UploadedFile
    :<|> UserAPI

type UserAPI = "user" :> Capture "userid" Int :> Get '[JSON] User
    :<|> "user" :> "create" :> ReqBody '[JSON] UserDetails :> Post '[JSON] User 

initializeConnection::Configuration->IO Connection
initializeConnection conf= do
    connect
      defaultConnectInfo
           { 
            connectDatabase = dbName conf,
            connectUser = dbUser conf,
            connectPassword =dbPassword conf
           }
             

server::Connection -> Server API
server conn = getSwagger :<|> serveDirectoryWebApp "static" :<|> uploadImage  :<|> getUser :<|> createUser
    where getUser userid = do
            user <- liftIO $(query conn "select uid,name,age,email \
                                        \ from public.createuser where uid=?;" [userid]::IO [User])
            return $head user
          -- | Add user to database
          createUser userDetails = do
            userid' <- liftIO $(query conn "insert into public.createuser(name,age,email) \
                                            \ values (?,?,?) returning uid;" userDetails ::IO [Only Int])
            let userid = fromOnly $head $userid'
            return $User userid (DbGetconfig.name userDetails) (age userDetails) (DbGetconfig.email userDetails) (Just "") 
          
          -- | get the swagger api spec
          getSwagger = return (toSwagger userApi)

          -- | upload an image to user profile
          uploadImage multipartData = do
            liftIO $ do
                let fileInRequest = lookupFile "file" multipartData 
                response <-  process fileInRequest
                let uploaded_files = uploadedFiles response
                let file = head uploaded_files 
                print (fmap fdPayload fileInRequest)
                return file
                    where process (Left _) = error "Invalid Request" 
                          process (Right file) = runReq defaultHttpConfig $createRequest (fdPayload file) 


createRequest content = do
    multiPartBody <-  reqBodyMultipart [partFileSource "files[]" content]
    res <- req Network.HTTP.Req.POST (https "tmp.ninja" /: "upload.php") ( multiPartBody) jsonResponse mempty
    return (responseBody res:: UploadResponse)
    

api ::Proxy API
api = Proxy

userApi :: Proxy UserAPI
userApi = Proxy

app::Connection -> Application
app conn = serve api (server conn)

startApp::IO()
startApp = do
    print "Connecting to DB..."
    conf <- getConfig
    
    case conf of
        Right conf-> do
            conn <-initializeConnection conf
            run 8080 (app conn)
            print "ok"
        Left error-> print"error occured to reach the db connection from startApp"
    print "Server running on port 8080"
    

-- $env:host = "postgres"
-- $env:user = "postgres"
-- $env:password = "Minnus"
