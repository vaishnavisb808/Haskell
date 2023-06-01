import Control.Concurrent
import Control.Concurrent.Chan
import System.Directory
waitExample = do
 ch <- newChan
 forkIO $ do
   writeFile "xyzzy1" "seo craic nua!"
   writeChan ch ("done with writing")
 readChan ch >>= print
 doesFileExist "xyzzy1"