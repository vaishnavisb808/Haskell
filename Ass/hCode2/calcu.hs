import System.Environment
--import Control.Monad
main ::IO()
main = do
  putStrLn "First number"
  num1 <- getLine
  putStrLn "Second number"
  num2 <- getLine
  putStrLn "ente the operator"
  op <- getLine
  let first = read num1::Int
  let second = read num2::Int  
  let result
          | op == "+" = show(first+second)
          | op == "-" = show(first-second)
          | op == "*" = show(first*second)
          | otherwise ="invalid"
  print result
   