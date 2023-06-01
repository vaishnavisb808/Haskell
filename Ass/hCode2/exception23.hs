import Control.Exception
--main = do
 --result <- try (evaluate (5 `div` 0)) :: IO (Either SomeException Int)
 --case result of
   --Left ex -> putStrLn $ "Caught exception " ++ show ex
   --Right ex -> putStrLn $ "Reslt is " ++ show ex

main = catch ( print $ 5 `div` 0) handler
  where
    handler :: SomeException -> IO()
    handler ex = putStrLn $ "Caught exception " ++ show ex
