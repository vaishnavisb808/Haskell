import Data.List
import Data.Maybe

gamma [] = ""
gamma (x:xs) = do  
      let y =length $ filter (=='0') x
      if y > (length x-y) then "0"++gamma xs
       else "1"++gamma xs



epsilon [] = ""
epsilon (x:xs) = do  
      let y =length $ filter (=='0') x
      if y < (length x-y) then "0"++epsilon xs
       else "1"++epsilon xs


bintodec :: Integral i => i -> Maybe i
bintodec 0 = Just 0
bintodec i | last < 2 = fmap (\x -> 2*x + last) (bintodec (div i 10))
           | otherwise = Nothing
    where last = mod i 10

main = do
    input<- fmap lines (readFile "input.txt")
    let y = transpose input
    let x = fromJust $ bintodec $ read $ gamma y
    let z = fromJust $ bintodec $ read $ epsilon y
    let mul = x * z
    print mul