module Util(splitBy) where
-- | split String into list of strings by given character
splitBy::Char->String->[String]
splitBy matchChar inpString =
    takeWhile (/=matchChar) inpString : remaining
                    where remainingString = dropWhile (/=matchChar) inpString 
                          remaining | null remainingString=[]
                                    | otherwise = splitBy matchChar (tail remainingString)