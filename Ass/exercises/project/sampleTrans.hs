{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
import Control.Applicative (liftA2,liftA3)
import Database.PostgreSQL.Simple
  ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute,query, Connection
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Control.Monad (forM_)
import Database.PostgreSQL.Simple.Time (Date)
import Data.Aeson
import Data.List(nub)
import GHC.Generics
--import Data.ByteString as B
import GHC.Generics (Generic)
--import System.Directory.Internal.Prelude (appendFile)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock (UTCTime)
-- |data types and its instances declared here
data Product = Product{product_id::Int ,product_name::String,product_quantity::Int,producct_price::Int}deriving (Show,Eq,Generic)
data Transaction=Transaction{time::UTCTime,transPrice::Int ,ttransaction_id::Int,product::[Product] }deriving (Show,Generic,Eq)

data User = User {orginal_rate :: Int, total_price :: Int,tids::Int} deriving (Show)
instance FromRow User where
   fromRow = liftA3 User field field field
instance ToJSON Transaction where
  toJSON(Transaction time transPrice ttransaction_id product)=
    object ["time" .= time,
    "total_price" .= transPrice, 
     "transaction_id" .= ttransaction_id,
     "product" .= product]

instance ToJSON Product where
  toJSON(Product  product_id product_name product_quantity product_price)=
    object ["products_id" .= product_id, 
    "product_name" .= product_name, 
    "product_quantity" .= product_quantity, 
    "product_price" .= product_price]

main :: IO ()
main = do
  conn <-
    connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "Minnus"
        }
  print "enter the date"
  date<-getLine
  -- | using the entered date computing price from product table, then it compared to the value that given to transaction table
  mm <- query conn "SELECT SUM (product_table.product_quantity * product_table.product_price) ,\
  \transaction_table.total_price,transaction_table.transaction_id FROM product_table INNER JOIN transaction_table ON\
  \ product_table.transaction_id=transaction_table.transaction_id WHERE transaction_table.time=? GROUP BY total_price,transaction_table.transaction_id" [date] ::IO[User]
  print mm
  comparePrice mm
-- |comparing the prices in this function
comparePrice :: [User] -> IO ()
comparePrice xs = if not(null xs)
     then do
          let original1 = orginal_rate (head xs)
          let back_price1 = total_price (head xs)
          let tid= tids (head xs)
          if original1==back_price1 then print "correct"
          else do
               print "MISMATCH ! details given to json file"
               conn <-
                 connect
                    defaultConnectInfo
                         { connectDatabase = "postgres",
                        connectUser = "postgres",
                        connectPassword = "Minnus"
                        }
              -- |if a mismatch msg found, then taking all the details from product and transaction table
               q <- query conn "SELECT time,total_price,transaction_table.transaction_id,product_id,\
               \product_name,product_quantity,product_price FROM transaction_table INNER JOIN product_table ON \
               \transaction_table.transaction_id=product_table.transaction_id WHERE transaction_table.transaction_id=?"[tid]
               let aproduct=fmap(\(_,_,_,product_id,product_name,product_quantity,producct_price)->Product product_id product_name product_quantity producct_price) q
               let trans =(map(\(time,transPrice,ttransaction_id,_,_,_,_)->Transaction time transPrice ttransaction_id aproduct) q)
               let trans'= encode(nub trans)
               --let etrans=encode trans'
               forM_ (aproduct, trans') $ \prdct -> B.appendFile "test1.json"  (prdct)
               --appendFile "test.json" "\n"
                -- |writing it to a file
              
          comparePrice (tail xs) 
          --appendFile -- |continues checking for all the transaction inthat selected date
          else
            print "out of range" -- |when the list end 