import System.Environment
import Control.Monad
main :: IO()
main = do
  putStrLn "enter string"
  lstring<- getLine
  let first = read therdc::String
  lthree :: String ->Char
  lthree x = drop 4(take 3 x)
  print (lthree)

--module Rweverse where
--rvrs :: String -> String
--rvrs x = drop 4 (take 5 x)
--main :: IO()
--main = print (rvrs "curry is awesome")