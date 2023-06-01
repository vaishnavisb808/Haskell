--return diffDays today dob
import Text.XML.HXT.Core
import Data.Time.Clock
import Data.Time.Calendar

main :: IO ()
main = do
  input <- readFile "userInput.xml"
  getdob <- runX $ readString [withValidate no] input 
        //> hasName "dob" 
        //> getText
  print getdob
  getname' <- runX $ readString [withValidate no] input 
        //> hasName "name" 
        //> getText
  print getname'
  let tto= read getdob::[(Integer, Int, Int)]
  cdate = date' tto
  days'= mapM_ ((-)cdate) tto
  ldays= ascOredr days'
  zipper= Map.fromList (zip getname' ldays)
  getdata'= ldays' getname' = Map.lookup getname' zipper
  print getdata' 

date' :: IO (Integer, Int, Int) -- :: (year, month, day)
date' = getCurrentTime >>= return . toGregorian . utctDay

ascOredr :: [Int] -> [Int]
ascOredr [] = []
ascOredr (x:y:xs) 
    | y > x = ascOredr (y:xs)
    | otherwise = ascOredr (x:xs)
