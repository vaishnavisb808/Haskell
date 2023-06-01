{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as Text
import Crypt
import Database.PostgreSQL.Simple
 ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute,
    query, Connection,
  ) 
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Control.Monad
import Database.PostgreSQL.Simple.Time (UTCTimestamp)
data Address = Address {address::Maybe String,pincode::Maybe String,state ::Maybe String,city ::Maybe String,addresstyp::Maybe String,cDate::UTCTimestamp,mDate ::UTCTimestamp}deriving (Show)
data User = User { userId ::Int,username :: String,addrs::[Address] } deriving (Show)

main = do
    Prelude.putStrLn"enter user name"
    name <- getLine
    let rr = funcQuery name
    aa<- addressTest rr
    print aa

funcQuery:: String->User
funcQuery name =do
    conn <-
     connect
      defaultConnectInfo
        { connectDatabase = "postgres",
         connectUser = "postgres",
          connectPassword = "Minnus"
        }
    resp <- query conn  "select login.userid,uname,address,pincode,state,city,addresstype,createddate,modifieddate from login inner join addresstable on login.userid = addresstable.userid where uname =  ?" [name ]
    print resp

addressTest::User->IO()
addressTest resp= do
    let addres=fmap(\(_,_,address,pincode,state,city,addresstyp,cDate,mDate)->Address address pincode state city addresstyp cDate mDate) resp
    let user =(\(userId,username,_,_,_,_,_,_,_)->User userId username addres) $ head resp
    forM_ (addres, user) $ \addr ->print addr
