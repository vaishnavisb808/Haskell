{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Headache where
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Control.Monad.IO.Class
import GHC.Generics
import Data.Aeson
import Network.HTTP.Req
--import OriginalImageHandler
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.ByteString.Char8 as Bytechar

-- data ImageDetails = ImageDetails{
--     confidence :: Double,
--     label :: String
--     } deriving(Generic,Show)

-- data ImageDetails' = ImageDetails'{
--     imageId::Int,
--     tags :: [ImageDetails]
--     } deriving(Generic,Show)

-- data ImageDetails'' = ImageDetails'' {
--     result :: [ImageDetails']
--    } deriving(Generic,Show)

-- data InputData= IinpuData {
--     url::String}
--     deriving(Generic,Show)
-- data Imagga= Imagga{
--     label::String,
--     image64::String,
--     detection::Char
-- }
-- instance FromJSON Imagga
-- instance ToJSON Imagga
-- instance FromJSON ImageDetails''
-- instance FromJSON ImageDetails'
-- instance FromJSON ImageDetails

-- data InputData = InputData{
--     url :: String,
--     label1 :: String,
--     enable :: String  
-- }deriving(Generic,Show)

-- instance FromJSON InputData

type UserAPI = "images" :> ReqBody '[JSON] InputData :> Post '[JSON] ImageDetails

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy UserAPI
api = Proxy

server :: Server UserAPI
server = imageHandler

-- sHandler :: Headache.InputData -> Handler ImageDetails
-- sHandler inpdata = do
--     hand <- liftIO(imageHandler inpdata)
--     return hand







