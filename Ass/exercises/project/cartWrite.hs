import Control.Monad
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe
products = ["pen", "pencil", "book"]
prices = [10, 5, 20]
productMap = Map.fromList (zip products prices)
addToCart :: String -> Writer [String] (Maybe Int)
addToCart pdct = do
  let p = Map.lookup pdct productMap
  let outputString =
        if p == Nothing
          then "Invalid product " ++ pdct
          else pdct
  writer (p, [outputString])
addItemsToCart shoppingList = do
  let pList = map addToCart shoppingList
  let total = foldr1 addProducts pList
  total
addProducts ::
  Writer [String] (Maybe Int) ->
  Writer [String] (Maybe Int) ->
  Writer [String] (Maybe Int)
addProducts a b = do
  a' <- a
  b' <- b
  let val
        | (a' == Nothing && b' == Nothing) = Just 0
        | (a' == Nothing && b' /= Nothing) = b'
        | (a' /= Nothing && b' == Nothing) = a'
        | otherwise = Just (extract a' + extract b')
  return val
extract (Just a) = a
shoppingList = ["pen", "book", "fruits"]
main = print $ runWriter (addItemsToCart shoppingList)