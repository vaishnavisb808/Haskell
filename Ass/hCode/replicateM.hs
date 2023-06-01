import System.Environment
import Control.Monad
main :: IO()
main = do
  args <- getArgs
  let linesToRead = if length args > 0
                    then read (head args)
                    else 0 :: Int       
  -- replicateM: repeat linesToRead getLine number of times
  numbers <- replicateM linesToRead getLine
  -- make a list of all the inputs that came from getLine
  let ints= map read numbers :: [Int]
  print (sum ints)