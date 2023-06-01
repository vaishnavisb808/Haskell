module PoemLines where

myLines :: String -> [String]
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\\ symmetry?"
sentences = show [firstSen ++ secondSen ++ thirdSen ++ fourthSen]
myLines inpString 
 |not (null inpString)=takeWhile (/='\n') inpString:
      if null(remainingString)||(remainingString=="\n")
       then[]
      else myLines(tail(remainingString)) 
 |otherwise=[inpString] where remainingString = dropWhile (/='\n')inpString 
--shouldEqual = "Tyger Tyger, burning bright" , "In the forests of the night" ,"What immortal hand or eye" , "Could frame thy fearful symmetry?"

main :: IO ()

main = do 
     print $ "Are they equal? " ++ show (myLines sentences == sentences) 