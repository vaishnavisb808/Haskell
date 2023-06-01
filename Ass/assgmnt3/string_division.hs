myWords :: String ->[String]
myWords inpu 
             | takeWhile (/=' ') inpu
                mywords(tail(remainins))
             | otherwise=[inpu] where remains = dropWhile (/=' ') inpu                 
--if null(remainingString) 
--  then []                         
--else 
--myWords(tail(remainingString))                  
              