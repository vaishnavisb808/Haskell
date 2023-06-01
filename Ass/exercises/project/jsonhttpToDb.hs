{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import Data.Aeson
import Network.HTTP.Req
import GHC.Generics
import qualified Text.URI as URI
import Database.PostgreSQL.Simple.ToRow
import Data.Map as M
import Database.PostgreSQL.Simple
 ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    ToRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute,
    query, executeMany,
  ) 
data User = User {
                    name :: String,
                    email :: String,
                    age ::Int,
                    phone::Int,
                    designation::String
                   }deriving (Show,Generic)
--data Address = Address {address:: String,pincode::String,state :: String

instance FromJSON User where
 parseJSON (Object v) =
    User   <$> v .: "name"
           <*> v .: "email"
           <*> v .: "age"
           <*> v .: "phone"
           <*> v .: "designation"
instance ToRow User

main :: IO()
main = do
        result <- runReq defaultHttpConfig $ do
                                    r <-
                                        req
                                        GET
                                        (https "jsonkeeper.com" /: "b" /: "Y5NE") 
                                        NoReqBody -- use built-in options or add your own
                                        jsonResponse -- specify how to interpret response
                                        mempty -- query params, headers, explicit port number, etc.
                                    return (responseBody r::[User])
        conn <-
           connect
                defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
        inp <-executeMany conn "insert into jsoninput (name,email,age,phone,designation) values (?,?,?,?,?)" (result)
        print inp