import Data.List
import Data.Maybe

mcb (x:xs) = do
      let y =length $ filter (=='0') x
      if y > (length x-y) then '0' else if y == (length x-y) then '1' else '1'

lcb (x:xs) = do
      let y =length $ filter (=='0') x
      if y < (length x-y) then '0' else if y == (length x-y) then '0' else '1'

funcList :: Char -> Int -> [String] -> [String]
funcList num index [] = []
funcList num index (x:xs)
   |  x !! index == num = x : funcList num (index) xs
   | otherwise = funcList num (index) xs

oxyGenerator [x] _ _ = [x]
oxyGenerator li x index = do
    let y = transpose  li
    let droppedtranslist = drop x y
    if droppedtranslist /= [] then do
     let max = mcb droppedtranslist
     let new = funcList max (index+1) li
     oxyGenerator (new) (x+1) (index+1)
     else return $ head li

co2Scrub [x] _ _ = [x]
co2Scrub li x index = do
    let y = transpose  li
    let droppedtranslist = drop x y
    if droppedtranslist /= [] then do
     let max = lcb droppedtranslist
     let new = funcList max (index+1) li
     co2Scrub (new) (x+1) (index+1)
     else return $ head li

bintodec :: Integral i => i -> Maybe i
bintodec 0 = Just 0
bintodec i | last < 2 = fmap (\x -> 2*x + last) (bintodec (div i 10))
           | otherwise = Nothing
    where last = mod i 10

main = do
    list <- fmap lines (readFile "input.txt")
    let y = transpose list
    let z = mcb y
    let t = funcList z 0 list
    let oxy = fromJust $ bintodec $ read $ head $ oxyGenerator t 1 0
    print oxy
    let e = lcb y
    let h = funcList e 0 list
    let co2 =  fromJust $ bintodec $ read $ head $ co2Scrub h 1 0
    print co2
    let mul = oxy * co2
    print mul