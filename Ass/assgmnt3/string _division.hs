myWords :: String ->[String]
  myWords inpu |takeWhile (/=' ') inpu :                   
      --if null(remainingString) 
        --      then []                         
        --else 
		      --myWords(tail(remainingString))                  
               | otherwise=[inpu] where dropWhile (/=' ') inpu