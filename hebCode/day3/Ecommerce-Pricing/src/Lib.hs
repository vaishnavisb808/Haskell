{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Types
-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''User)

-- type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  return getLogic :<|> return postLogic
          where

                getLogic object= do
                  case object of
                    (Just a) -> print modifyRule
                    Nothing -> throwError myerr
                postLogic = throwError myerr

                myerr :: ServerError
                myerr = err503 { errBody = "Sorry dear user." }

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]
