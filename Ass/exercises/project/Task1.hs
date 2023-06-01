module Task1 where
import Data.List (delete,nub)
task1 :: Char -> String -> (String,Int)
task1 char string = do                
                 let removed_string= filter (/=char)string               
                 let count = (length string) - (length removed_string)               
                 (removed_string,count)
