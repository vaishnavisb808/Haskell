import Control.Concurrent
{-newMVar :: a -> IO (MVar a) M for mutable- to create mutable variable
newEmptyMVar ::IO (MVar a) -to create shared variable for other threads to read 
takeMVar :: Mvar a ->IO a -take until empty
putMVar :: MVar a -> a -> IO() - put until it full
isEmptyMVar :: MVar a ->IO Bool - check it empty or not
-}

communicate = do  -- is a blocking call that is until empty or full
  m <- newEmptyMVar --shared variable created
  forkIO $ do
    putMVar m "wake up!"
    putStrLn "sending"
  v <- takeMVar m -- takinf from m -- first no value in m, so it not perform take operation,
  putStrLn ("received " ++ show v)                     --so sending is executed that is wake up, that is main function execute frst,
                       -- last putSteln part of main , other is inner one
