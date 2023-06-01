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
import Data.List(nub)
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Time.Clock as T
--import Database.PostgreSQL.Simple.Time (Date)
import Database.PostgreSQL.Simple.Types (Only(fromOnly))
import GHC.Prelude (head)
-- |data types and its instances declared here
data Product = Product{product_id::Int ,product_name::String,product_quantity::Int,product_price::Int}deriving (Show,Generic)
data Transaction=Transaction{ttransaction_id::Int,transPrice::Int ,time::T.UTCTime,product::[Product] }deriving (Show,Generic)
data User = User {orginal_rate :: Int, total_price :: Int,tids::Int} deriving (Show)
instance FromRow User where
   fromRow = liftA3 User field field field
data Count= Count {count::Int} deriving (Show,Generic)
instance FromRow Count
instance ToJSON Transaction where
  toJSON(Transaction ttransaction_id transPrice time product)=
    object ["transaction_id" .= ttransaction_id,
     "total_price" .= transPrice, 
     "time" .= time, 
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
  
  print "aa"
  -- | using the entered date computing price from product table, then it compared to the value that given to transaction table
  
  --rr<- query_ conn "select count(*) from transaction_table inner join product_table on transaction_table.transaction_id =product_table.transaction_id "::IO [Count]
  --print rr
  --let c= count(head rr)
  --print c
  print "enetr the date"
  date<-getLine
  let d= read date ::T.UTCTime
  let i=0
  recursOffset i d
  --print r
  
recursOffset::Int->T.UTCTime->IO()
recursOffset y date
   | y >= 0=do
       conn <-
          connect
            defaultConnectInfo
             { connectDatabase = "postgres",
              connectUser = "postgres",
              connectPassword = "Minnus"
             }
       mm <- query conn "SELECT SUM (product_table.product_quantity * product_table.product_price) ,\
       \transaction_table.total_price,transaction_table.transaction_id FROM product_table INNER JOIN transaction_table ON\
       \ product_table.transaction_id=transaction_table.transaction_id WHERE transaction_table.time=? GROUP BY total_price,\
       \transaction_table.transaction_id ORDER BY transaction_table.transaction_id LIMIT 3 OFFSET ?" (date, y)::IO[User]
       print mm
       if (Prelude.null mm)
           then print "end"
           else do
               comparePrice mm
               recursOffset (y+3) date
   | otherwise =
            print "table over"
      
       

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
               q <- query conn "SELECT transaction_table.transaction_id,total_price,time,product_id,\
               \product_name,product_quantity,product_price FROM transaction_table INNER JOIN product_table ON \
               \transaction_table.transaction_id=product_table.transaction_id WHERE transaction_table.transaction_id=?"[tid]
               let aproduct=fmap(\(_,_,_,product_id,product_name,product_quantity,product_price)->Product product_id  product_name product_quantity product_price) q
               let trans =encode(map(\(ttransaction_id,transPrice,time,_,_,_,_)->Transaction ttransaction_id transPrice time aproduct) q)
               forM_ (aproduct, trans) $ \prdct -> B.appendFile "test1.json"  ( prdct)
               --appendFile "test.json" "\n"
                -- |writing it to a file
              
          comparePrice (tail xs) 
          --appendFile -- |continues checking for all the transaction inthat selected date
          else
            print "out of range" -- |when the list end 