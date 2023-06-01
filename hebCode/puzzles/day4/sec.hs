{-# LANGUAGE OverloadedStrings #-}
import           Data.Binary.Get (remaining)
import           Data.List
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
main = do
    (bingoCallOrder,bingoSheets) <- readBingoFile "input4.txt"
    let sums=[]
    playBingo bingoCallOrder bingoSheets sums
-- | read the file and create necessary haskell types
readBingoFile filePath = do
    bingoSheet <- TIO.readFile filePath
    let bingoSheetLines = filter (\x -> not (T.null x)) $T.lines bingoSheet
    let bingoCallOrder = toInt $head bingoSheetLines
    let bingoSheets = getBingoSheets $tail bingoSheetLines
    return (bingoCallOrder,bingoSheets)
  where
    toInt::T.Text->[Int]
    toInt text = map (read.T.unpack) (T.splitOn "," text)
-- | mark the values one by one in all the sheets until one of them gets a marked row or
-- column

playBingo :: [Int] -> [[[Int]]] -> [Int] -> IO ()
playBingo _ [] sums= print (head sums)
playBingo [] _ sums= print (head sums)
playBingo (currentCall:remainingCalls) bingoSheets sums= do
    let markedSheets = strike currentCall <$> bingoSheets
    let status = isBingo <$> markedSheets
    case elemIndex True status of
      Just index -> do
          let newSums =(currentCall * sumOfUnMarked (markedSheets!!index)):sums
          let newSheet =take index markedSheets ++ drop (index+1) markedSheets
          playBingo (currentCall:remainingCalls) newSheet newSums
      Nothing    -> playBingo remainingCalls markedSheets sums
-- | find the sum of all values in a Bingosheet that are not marked yet
-- i.e sum of values that are not -1
sumOfUnMarked :: [[Int]] -> Int
sumOfUnMarked [] = 0
sumOfUnMarked (head:rest) = sumOfRow head + sumOfUnMarked rest
    where
        sumOfRow []            = 0
        sumOfRow (val:restVal) = nullifyMarked val + sumOfRow restVal
        nullifyMarked value | value == -1 = 0
                            | otherwise = value
isBingo :: [[Int]] -> Bool
isBingo bingoSheet = horizontalBingo || verticalBingo
    where
        horizontalBingo = isBingo' bingoSheet
        verticalBingo   = isBingo' $ transpose bingoSheet
        isBingo' = any (\ x -> sum x == - 5)

-- | Find the given number in the bingo sheet and replace it with -1 if found
strike :: Int -> [[Int]] -> [[Int]]
strike value = fmap (fmap strike')
    where
        strike'::Int->Int
        strike' x
          | x==value  = -1
          | otherwise = x
-- | convert a row from the bingosheet to a list of ints
getBingoSheetRow :: T.Text -> [Int]
getBingoSheetRow bingoSheetLine = readAsInt <$> splitBySpace
  where
    readAsInt = read.T.unpack
    splitBySpace = filter (not.T.null) (T.splitOn " " bingoSheetLine)
-- | convert the textFormat bingoSheet to intFormat bingoSheet [[Int]]
getBingoSheets [] = []
getBingoSheets bingoSheetLines = (getBingoSheetRow <$> fiveLines):getBingoSheets remaining
    where
        fiveLines = take 5 bingoSheetLines
        remaining = drop 5 bingoSheetLines