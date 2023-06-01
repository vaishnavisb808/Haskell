import System.Environment
toInts :: String -> [Int]
toInts = map read.lines
main :: IO()
main = do
  userinput <- getContents
  let numbers= toInts userinput 
  print (sum numbers)

-- it will comtinue to take contents until u sent an ctrl+Z