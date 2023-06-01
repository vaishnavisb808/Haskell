{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import Data.Aeson
import Network.HTTP.Req
import GHC.Generics
import qualified Text.URI as URI
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import Data.Map as M
import Database.PostgreSQL.Simple
 ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    ToRow,
    FromRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute,
    query, executeMany,
  ) 
import Data.Map.Internal
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow))
import System.Directory.Internal.Prelude (Monad(return))
import Database.PostgreSQL.Simple.Types (Only(Only, fromOnly))
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Transaction
import Database.PostgreSQL.Simple.ToField (ToField(toField))
import GHC.Cmm.Opt (cmmMachOpFoldM)
import GHC.RTS.Flags (TraceFlags(user))
import qualified Data.Map.Lazy as M
data Address = Address {addresstype :: String,address:: String,pin::Int}deriving (Show,Generic)
data User = User {
                    name :: String,
                    email :: String,
                    age ::Int,
                    phone::Int,
                    designation::String,
                    addresses::[Address]
                   }deriving (Show,Generic)

instance FromJSON User where
  parseJSON (Object v) = User
   <$> (v.: "name")
   <*> (v.: "email")
   <*> (v.: "age")
   <*> (v.: "phone")
   <*> (v.: "designation")
   <*> (v.: "addresses")
instance FromJSON Address where
  parseJSON (Object v) = Address
   <$> (v.: "addresstype")
   <*> (v.: "address")
   <*> (v.: "pin")
instance ToRow User where
  toRow (User name email age phone designation addresses)=[toField name,toField email,toField age, toField phone, toField designation]

main :: IO()
main = do
        result <- runReq defaultHttpConfig $ do
                                    r <-
                                        req
                                        GET
                                        (https "jsonkeeper.com" /: "b" /: "O94K") 
                                        NoReqBody -- use built-in options or add your own
                                        jsonResponse -- specify how to interpret response
                                        mempty -- query params, headers, explicit port number, etc.
                                    return (responseBody r::[User])
        mapM_ func result
func user = do
  conn <-
     connect
            defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
        
  {-r<-query conn "insert into jsoninput (name,email,age,phone,designation) values (?,?,?,?,?)returning userid" user 
  print (r::[Only Int])
  let userID = fromOnly (head r)
  print userID
  mm<- executeMany conn "insert into jsonaddress(userid,address,addresstype,pin)values (?,?,?,?)"$ Prelude.map (addresswith userID)(addresses user)      
  print mm-}
  withTransaction conn $ do
    r<-query conn "insert into jsoninput (name,email,age,phone,designation) values (?,?,?,?,?)returning userid" user 
    --print (r::[Only Int])
    let userID = fromOnly (head r)
    print r
    mm<- executeMany conn "insert into jsonaddress(userid,address,addresstype,pin)values (?,?,?,?)"$ Prelude.map (addresswith userID)(addresses user)      
    print mm
addresswith ::Int ->Address ->(Int,String,String,Int)
addresswith userID (Address address addresstype pin) = (userID,address,addresstype,pin)     
    