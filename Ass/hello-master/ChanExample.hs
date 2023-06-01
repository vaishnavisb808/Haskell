import Control.Concurrent
import Control.Concurrent.Chan

chanExample = do -- its is a non blocking call,here main is force to wait, in others are main is first so get false
  ch <- newChan  -- put Chan in main to wait part it will wait until the heavy wrk cmpltd
  forkIO $ do
    writeChan ch "hello world"
    writeChan ch "now i quit"
  readChan ch >>= print
  readChan ch >>= print