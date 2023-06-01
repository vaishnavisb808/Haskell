{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative (liftA2)
import Database.PostgreSQL.Simple
  ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,
    Only,
    connect,
    defaultConnectInfo, 
    query_,
    execute,
    query,fromOnly
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)

data Products = Products {product_id :: Int,transaction_id:: Int,product_name::String,product_quantity::Int,product_price::Int} deriving (Show)
instance FromRow Products where
   fromRow = Products <$> field <*> field <*> field <*> field <*> field 

data Product = Product {orginal :: Int, back_price :: Int,ids::Int} deriving (Show)
instance FromRow Product where
   fromRow = Product <$> field <*> field <*> field 

missedRate :: Int -> IO ()
missedRate id1= do
    conn <-
     connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "kavya"
        }
    --print id1
    ys<- (query conn "select * from product_table where transaction_id=?" [id1]) :: IO[Products]
    print ys
    writeFile "test.json" (show(ys))
    --print "mismatch"

comparePrice :: [Product] -> IO ()
comparePrice xs = if not(Prelude.null xs)

     then do
          let original1 = orginal (head xs)
          --print original1
          let back_price1 = back_price (head xs)
         -- print back_price1
          
          if original1==back_price1 then print "correct"{-++original1 ++"is"++back_price1-}
          else do 
               let id1 = ids (head xs)
               print id1
               missedRate id1
          comparePrice (tail xs)
          
          else
            putStrLn "out of range"
main :: IO ()
main = do
  conn <-
    connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "Minnus"
        }
 
  --mapM_ print =<< (query_ conn "select sum (product_table.product_quantity * product_table.product_price),transaction_table.total_price from product_table inner join transaction_table on product_table.transaction_id=transaction_table.transaction_id where transaction_table.transaction_time='2021-11-01' group by total_price;" :: IO [User])
  xs<- (query_ conn "select sum (product_table.product_quantity * product_table.product_price),transaction_table.total_price,transaction_table.transaction_id from product_table inner join transaction_table on product_table.transaction_id=transaction_table.transaction_id where transaction_table.time='2021-10-12' group by total_price,transaction_table.transaction_id;" :: IO [Product])
  print xs
  comparePrice xs
{-
select (product_quantity * product_price) as hai from product_table;
select sum(product_quantity) as hai from product_table where transaction_id =2;
select sum(product_quantity * product_price) as hai from product_table where transaction_id =2;-}

--select sum (product_table.product_quantity * product_table.product_price),transaction_table.total_price from product_table inner join transaction_table on product_table.transaction_id=transaction_table.transaction_id where transaction_table.transaction_time='2021-11-01' group by total_price;