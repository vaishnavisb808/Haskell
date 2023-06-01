{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.ByteString.Lazy
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
type RootEndpoint =
  Get '[JSON] User
type API = Get '[JSON] [User]  -- "users" :> "list-all" :> Get '[JSON] [User]
             :<|>  "errors" :> Get '[JSON] [User]
--type SinleAPI = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

failingHandler :: Handler ([User])
failingHandler = throwError myerr

  where myerr :: ServerError
        myerr = err503 { errBody = "Sorry dear user." }

-- server :: Server SingleAPI
-- server = return users 
server :: Server API
server =  failingHandler
      :<|> return users


users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
