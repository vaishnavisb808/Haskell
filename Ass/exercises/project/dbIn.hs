{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Control.Applicative ( liftA3 )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple
import Data.Time
import Data.Int
import Crypto
data User = User { getUsername :: String, getEmail :: String, getPassword::String} deriving Show
instance FromRow User where
        fromRow = liftA3 User field field field 
                
main :: IO ()
main = do
        conn <- connect defaultConnectInfo
                { 
                  connectDatabase = "postgres"
                , connectUser = "postgres"
                , connectPassword = "Minnus"
                }
        print "name :"
        name<-getLine
        print "email :"
        email<-getLine
        print "password :"
        password<-getLine
        xs <- execute conn "insert into public.login (uname, email, psswrd) values (?, ?, ?)" [name, encrypt email, encrypt password] 
        if(xs == 1)
            then putStrLn $ show xs ++ " new user created " 
            else print "Unable to create user"