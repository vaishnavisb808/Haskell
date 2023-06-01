import Data.Time
import System.IO

--getTime' :: IO UTCTime
--getTime'
--data IntTime = IntTime (Int,String ) deriving (Show)
summation ::Int -> Int->Int
summation a b= a + b
 
applyLog :: String ->Int -> IO()
applyLog val' sum' = do 
    now <- getCurrentTime
    let outfile = "output1.txt"
    let val = ("\n time of operation\t"++ show now ++"\t"++ val'++"sum = " ++ show sum')
    appendFile outfile val

--difference :: Int-> Int -> Int
--difference a b = a - b
 
main ::IO()
main = do
    putStrLn "Enter Integer to add"
    a <- getLine
    let a'= read a :: Int
    b <- getLine
    let b' = read b :: Int
    let summ = summation a' b'
    applyLog "\t Addion performed \t" summ