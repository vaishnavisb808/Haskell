import System.Environment
main ::IO()
main = do
    vals <- (mapM_ (\x->getLine) [1..3])
    print vals
    --args <- getArgs 
    --mapM_ putStr (fmap (" "++) args)
-- to run thi runghc .\importSystemEnvironment29.hs hi hello
{- putstr with space using fmap  

importSystem.Environment
main ::IO()
main = do
    args <- getArgs 
    mapM_ putStr (fmap (" "++) args)

    importSystem.Environment
main ::IO()
main = do
    args <- getArgs 
    mapM_ putStrLn args
with new line

-}