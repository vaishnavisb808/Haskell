{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative (liftA2)
import Database.PostgreSQL.Simple
  ( ConnectInfo (connectDatabase, connectPassword, connectUser),
    FromRow,
    Only,
    connect,
    defaultConnectInfo,
    query_,
    execute,query
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
data User = User {orginal_rate :: Int, total_price :: Int} deriving (Show)
instance FromRow User where
   fromRow = liftA2 User field field
main :: IO ()
main = do
  conn <-
    connect
      defaultConnectInfo
        { connectDatabase = "postgres",
          connectUser = "postgres",
          connectPassword = "Minnus"
        }
  
  mm <- (query_ conn "select sum (product_table.product_quantity * product_table.product_price) ,transaction_table.total_price from product_table inner join transaction_table on product_table.transaction_id=transaction_table.transaction_id where transaction_table.time='2021-10-12' group by total_price;" :: IO [User])
  print mm
  comparePrice mm

comparePrice :: [User] -> IO ()
comparePrice xs = if not(Prelude.null xs)
     then do
          let original = orginal_rate (head xs)
          --print original
          
          let transaction_price = total_price (head xs)
          --print transaction_price
          if (original==transaction_price)
              then print "tally"
              else print "mismatch in price"
          comparePrice (tail xs)
          else
              putStrLn "list end"
