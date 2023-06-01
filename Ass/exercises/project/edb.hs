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
  
  conn <-
    connect
      defaultConnectInfo
        { connectDatabase = "postgres",
         connectUser = "postgres",
          connectPassword = "Minnus"
        }
  dataB <- (query conn "select email from login where email = ?" [eemail]) :: IO [Only String]
  if not(Prelude.null dataB)
  then Prelude.putStrLn "Already exists"
  else do
  insertR <- execute conn "insert into login (uname, email, psswrd) values (?, ?, ?)"(name :: String, eemail :: String, epsswrd :: String)
  if(insertR == 1)
  then Prelude.putStrLn $ show insertR ++ " new user created "
  else print "Unable to create user"