import qualified Data.Text as Tx
import qualified Data.List.NonEmpty as Tx

main :: IO ()
main = do
     print $ Tx.splitOn (Tx.pack ",") (Tx.pack "example for splitOn,and splitAt, in , haskell")
     print $ splitAt 5  [1..10]