import System.IO
import Data.Char(toUpper)
import System.Environment

main :: IO()
main = do
  args <- getArgs
  inh <- openFile (args !! 0) ReadMode
  outh <- openFile (args !! 1) WriteMode
  inpStr <- hGetContents inh
  let result = processData inpStr
  hPutStr outh result
  hClose inh
  hClose outh
processData :: String -> String
processData = map toUpper

-- to run this runghc lazyiowithgetArgs.hs myname.txt out.txt
-- same as the readWriteGetContents.hs