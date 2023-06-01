myWords::String-> [String]
myWords inpData 
 |not (null inpData)=takeWhile (/=' ') inpData:  
  if null(dataRemains)   
   then[]  
  else myWords(tail(dataRemains))
 | otherwise=[inpData] 
 where dataRemains = dropWhile (/=' ') inpData