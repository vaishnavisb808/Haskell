wordSplit :: String ->[String]
wordSplit input | not(null input) =word
                | otherwise = ["empty string"]
                where word = words input 

reverseofString :: String -> [String]
reverseofString word = reverse (wordSplit word) 

combineString :: String -> String
combineString word =  foldl (++) " " (reverseofString word)
