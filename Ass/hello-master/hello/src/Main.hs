module Main where
import Hello
--import DogsRule
main :: IO ()
main = do
  putStrLn "please enter your name"
  name<- getLine
  sayHello name
  --sayHi
--    dogs