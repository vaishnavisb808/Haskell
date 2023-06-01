{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Createuser
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.ByteString.Lazy
import Control.Monad.IO.Class
import GHC.Int
import Database.PostgreSQL.Simple
 ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,ToRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute, 
    query,
  ) 
data User = User
  { name  :: String
  , age :: Int
  , email  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
type RootEndpoint =
  Get '[JSON] User

type SingleAPI = "createuser" :> ReqBody '[JSON] User :> Post '[JSON] String

--instance FromJSON User
--instance ToRow User

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
              data'<-execute conn "insert into createuser (name,age,email) values (?,?,?)"(name'::String, age'::Int,email'::String )
              return data'
              
succesHandler :: User->Handler String
succesHandler user= do 
    val<- liftIO (querying user)
    if val==1
        then return "success"
        else return "failed"

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy SingleAPI
api = Proxy

server :: Server SingleAPI
server =  succesHandler

