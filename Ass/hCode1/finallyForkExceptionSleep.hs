import Control.Concurrent

test1 = do
    tid <- forkIO $ threadDelay 10000000
    killThread tid
--
test2 = do
    tid <- forkIO $ undefined
    killThread tid

test3 = do -- for all exception taking, the 'forkFinally' to perform the all action if the earlier is did or not, it not consider
    mvar <- newEmptyMVar
    tid <- threadDelay 5000000 `forkFinally` \_ -> putMVar mvar()
    takeMVar mvar


