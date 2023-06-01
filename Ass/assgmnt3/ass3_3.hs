splitBy :: String ->Char ->[String]
splitBy inpstring matchChar 
  |not (null inpstring)= takeWhile(/=matchChar)inpstring:  
    if null inpstring||(length remainingstg==1&&head remainingstg==matchChar)||    
    null (inpstring)||null(remainingstg) then []    
    else splitBy (tail ( remainingstg ) ) matchChar  
   | otherwise=[inpstring] 
     where remainingstg=dropWhile(/=matchChar)inpstring