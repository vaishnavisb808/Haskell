import Data.Char(toUpper)
import Control.Concurrent
main = do
    inpStr <- readFile "input.txt"
    ch <- newChan
    forkIO $ do
            writeFile "output.txt"(map toUpper inpStr)
            writeChan ch ("done with writing")
    readChan ch >>= print