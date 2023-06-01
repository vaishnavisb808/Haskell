{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE DeriveAnyClass #-}
module Shopping where

import qualified Control.Monad.IO.Class as M
import Data.Aeson
import Control.Applicative(liftA2)
import Data.Aeson.Types
import Data.Map as M
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Req 
import Data.Monoid
import GHC.Generics
import GHC.Hs (resultVariableName)
import GHC.SysTools.Process (runSomething)
import Database.PostgreSQL.Simple
  ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,executeMany,
    execute,
    Only,
    ToRow,
    connect,
    fromOnly,
    defaultConnectInfo,
    query,query_,
    rollback,withTransaction
	
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (toField)
import Database.PostgreSQL.Simple.ToRow
import Control.Exception
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.Transaction (withTransaction)

import Data.Time


data User = User {total_price::Int} deriving (Show)
instance FromRow User where
  fromRow = fmap User field




data User' = User' {product_price::Int,product_quantity::Int} deriving (Show)
instance FromRow User' where
    fromRow = User' <$> field <*> field



calcPrice :: [User'] -> Int
calcPrice xs = if not(Prelude.null xs) then (quant * price) + calcPrice (tail xs)
                else 0  where
                    quant = product_quantity(head xs)
                    price = product_price(head xs)


 


main :: IO()
main = do
    

    conn <-
        connect
            defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "Minnus"
                }
    id<- getLine
    let id' = read id ::Int
    priceData <- (query conn "select total_price from transaction_table where transactionid=?"[id]):: IO [Only Int]

    let totalPrice = fromOnly (head priceData)
    print totalPrice



    proData <- (query conn "select quantity,price from product_table where transactionid=?"[id])::IO[User']
    print (proData)

    let x = calcPrice proData
    print x

    if totalPrice /= x then print "Mismatch"
    else print "Tally"