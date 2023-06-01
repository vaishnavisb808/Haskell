import System.Environment
import Control.Monad

main :: IO()
main = do
  args <- getArgs
  let linesToRead  |length args > 0 read (head args)
			       |0::Int
  numbers <- replicateM linesToRead getLine
  let ints = map read numbers :: [Int]
  
  print (sum ints)