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



data Transactions = Transactions {
                        transaction_id::Int,
                        total_price::Int,
                        products::[Product']
                        }
                        deriving (Show)



data Product' = Product' {
                            -- product_id::Int,
                            -- product_name::String,
                            product_price::Int,
                            product_quantity::Int
                       }
                       deriving (Show)



actual_price::[Product']->Int
actual_price xs  = if not(Prelude.null xs) then p*q +actual_price (tail xs)
                   else 0 where
                       p = product_price(head xs)
                       q= product_quantity(head xs)




-- actual_prs :: [(a, b, c, d)]-> [Char]

-- actual_prs xs = if(not(Prelude.null xs)) then do
--                      let tID = (\(x,y,z,a)->x)(head xs)
--                      let tPrs = (\(x,y,z,a)->y)(head xs)
--                      let actual =((\(x,y,z,a)->z)(head xs))*((\(x,y,z,a)->a)(head xs))
--                      show actual
--                 else  "bvhb"



main :: IO()
main = do

    conn <-
        connect
            defaultConnectInfo
                { connectDatabase = "postgres",
                  connectUser = "postgres",
                  connectPassword = "adarsh123"
                }

    xs <- (query_ conn 
                "SELECT transaction_table.transaction_id,total_price, product_price, product_quantity FROM transaction_table INNER JOIN product_table ON transaction_table.transaction_id=product_table.transaction_id WHERE transaction_table.transaction_time='2020-12-24'"
                )
    print xs

    let products = fmap (\(_,_,product_price,product_quantity)->Product' product_price product_quantity)xs
    -- print products
    let trans = (\(transaction_id,total_price,_,_)-> Transactions transaction_id total_price products)$head xs
    let bill_price = total_price trans 
    let actual_amount=actual_price products
    print(actual_amount)
    -- let result = actual_prs xs


    -- print(result)