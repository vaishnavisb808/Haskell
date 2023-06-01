{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative 
import Crypt(encrypt)
import Database.PostgreSQL.Simple
 ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute, 
  ) 
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Data.ByteString.Char8 (putStrLn)
data User = User {username :: String, email :: String, password :: String, userId ::Int} deriving (Show)
instance FromRow User where
  fromRow =  User <$> field <*> field <*> field <*> field
main :: IO ()
main = do
  Prelude.putStrLn "enter username ,password, email \n"
  name <- getLine 
  upsswrd <- getLine 
  uemail <- getLine 
  let epsswrd = (encrypt upsswrd )
  let eemail = encrypt uemail 
  print name
  print epsswrd
  print eemail
  conn <-
    connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "Minnus"
        }
  execute conn "insert into public.login (email,uname,psswrd) values (?,?,?)" (eemail::String ,name::String,epsswrd::String)
  mapM_ print =<< (query_ conn "select * from public.login" :: IO [User])

  
