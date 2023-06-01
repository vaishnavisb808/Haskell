{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative (liftA3)
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
    query,
  ) 
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Data.ByteString.Char8 (putStrLn)
import Data.ByteString.Short (null)
--import Database.PostgreSQL.Simple.Vector.Unboxed (query_)
data User = User {username :: String, email :: String, password :: String, userId ::Int} deriving (Show)
instance FromRow User where
  fromRow =  User <$> field <*> field <*> field <*> field
main :: IO ()
main = do
  Prelude.putStrLn "enter username "
  name <- getLine 
  Prelude.putStrLn "enter password"
  upsswrd <- getLine 
  Prelude.putStrLn "enter email"
  uemail <- getLine 
  let epsswrd = (encrypt upsswrd )
  let eemail = encrypt uemail 
  --print name
  --print epsswrd
  --print eemail
  conn <-
    connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "Minnus"
        }
  --execute conn "insert into public.login (email,uname,psswrd) values (?,?,?)" (eemail::String ,name::String,epsswrd::String)
  resp <- query conn
           "SELECT userid,uname,email,psswrd FROM public.login WHERE psswrd = (?) AND uname = (?) AND email= (?)"
           (epsswrd ::String, name :: String, eemail::String ) :: IO [User]
  --let u = email resp
  print (resp) 
 {- if( Prelude.null resp )
     then do
       execute conn "insert into public.login (email,uname,psswrd) values (?,?,?)" (eemail::String ,name::String,epsswrd::String)
       print " new user created " 
     else 
       print "User Exist \n "
       
  --mapM_ print =<< (query_ conn "select * from public.login" :: IO [User])


  -}
