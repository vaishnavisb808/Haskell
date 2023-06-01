{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
module Update where
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Data.Aeson
import GHC.Generics
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple
import Control.Monad.IO.Class
import Servant.Swagger
import Data.Swagger
import GHC.Int

instance FromJSON User
instance ToJSON User
instance FromRow User
instance ToSchema User
instance FromJSON UserDetails
instance ToJSON UserDetails
instance ToRow UserDetails
instance ToSchema UserDetails
instance FromRow UserUpdate
instance ToSchema UserUpdate
instance ToRow UserUpdate
instance FromJSON UserUpdate
data User=User {userid::Int,uname::String,uage ::Int,uemail::String} deriving(Eq,Show,Generic)

data UserDetails= UserDetails {name'::String,age' ::Int,email'::String} deriving(Eq,Show,Generic)

data UserUpdate = UserUpdate {upname ::String}deriving (Generic,Eq,Show)

type API =
    "swagger.json":> Get '[JSON] Swagger
    :<|> "docs":> Raw
    :<|> UserAPI
    

type UserAPI = "user":> Capture "userid" Int :> Get '[JSON] User
               :<|> "user" :> "create" :> ReqBody '[JSON] UserDetails :> Post '[JSON] User
               :<|> "user" :> "update" :> Capture "id" Int :> ReqBody '[JSON] UserUpdate :>Post '[JSON] String

setConnection::IO Connection
setConnection =do 
    connect
                  defaultConnectInfo
                   { connectDatabase = "postgres",
                   connectUser = "postgres",
                   connectPassword = "Minnus"
                   }
userapi:: Proxy UserAPI
userapi= Proxy

server ::Connection -> Server API
server conn = getSwagger :<|> serveDirectoryWebApp "static" :<|> getUser :<|>createUser :<|>updateUser
     where getUser  uid= do
              user<- liftIO ((query conn "select uid,name,age,email from createuser where uid=?"[ uid])::IO [User])        
              return $ head user
           createUser userDetails = do
               userid'<-liftIO ((query conn "insert into public.createuser (name,age,email)\
                                          \values (?,?,?) returning uid" userDetails):: IO [Only Int])
               let uid= fromOnly (head userid')
               return $User uid(name' userDetails)(age' userDetails)(email' userDetails)
           --updateUser::Int->UserUpdate->Handler String
           updateUser uid userUpdate= do
               let uuname= upname userUpdate 
               userData<- liftIO ((execute conn "update createuser set name=? where uid=?"(uuname , uid))::IO GHC.Int.Int64)
               --return $ show userData
               if userData==1
                   then return"Updated"
                   else return"Failed"
           getSwagger = return(toSwagger userapi)

api::Proxy API
api =Proxy

app:: Connection ->Application
app conn= serve api (server conn)

startApp::IO()
startApp= 
    do
        conn<-setConnection
        print "server running"
        run 8080 (app conn)