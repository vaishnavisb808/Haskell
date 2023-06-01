{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module FromDB
    ( startApp
    , app
    ) where
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.ByteString.Lazy
import Control.Monad.IO.Class
import GHC.Int
import Control.Applicative(liftA3)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple


data User = User
  { name  :: String
  , age :: Int
  , email  :: String
  } deriving (Eq, Show,Generic)

$(deriveJSON defaultOptions ''User)
type RootEndpoint =
  Get '[JSON] User

type SingleAPI = "createuser" :> ReqBody '[JSON] User :> Post '[JSON] String
     :<|> "userid" :> Capture "uid" Int :> Get '[JSON] [User]

--instance FromJSON User
instance ToRow User 
instance FromRow User where
   fromRow = liftA3 User field field field 

querying::User->IO GHC.Int.Int64
querying user = do 
              let name'=name user
              let age'=age user
              let email'=email user  
              conn <-connect
                  defaultConnectInfo
                   { connectDatabase = "postgres",
                   connectUser = "postgres",
                   connectPassword = "Minnus"
                   }
              data'<-execute conn "insert into createuser name,age,email values (?,?,?)"(name'::String, age'::Int,email'::String )
              return data'
              
succesHandler :: User->Handler String
succesHandler user= do 
    val<- liftIO (querying user)
    if val==1
        then return "success"
        else return "failed"

fetchUsersHandler :: Int -> Handler [User]
fetchUsersHandler id' =do 
  vaal<-liftIO (fromDBquery id')
  return vaal
  
fromDBquery::Int->IO [User]
fromDBquery id' = do
     conn <-connect
                  defaultConnectInfo
                   { connectDatabase = "postgres",
                   connectUser = "postgres",
                   connectPassword = "Minnus"
                   }
     data''<- query conn "select name,age,email from createuser where uid=?" (Only id') :: IO [User]
     return data''

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy SingleAPI
api = Proxy

server :: Server SingleAPI
server =  succesHandler
         :<|> fetchUsersHandler

