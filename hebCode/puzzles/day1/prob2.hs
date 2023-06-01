import GHC.Generics
import System.IO
counts::Int ->Int 
counts i=do
    let j=i 
    let c = j+1
    c

main::IO()   
main=do
    numb<-fmap lines (readFile "input.txt")
    let cont=map (read::String -> Int)numb
    let i=0
    l<- threeCount cont i
    print l

threeCount ::(Monad m,Ord a,Num a) => [a] -> Int -> m Int
threeCount [] i=return i
threeCount [x1,x2,x3] i=return i
threeCount(x:xs) i=do
    let num1 = x
    let num2 = head xs
    let num3 = head (tail xs)
    let num4 = head (tail (tail xs))
    let sum1 = num1 + num2 + num3
    let sum2 = num2 + num3 + num4
    if ((sum2 - sum1) > 0) then do
           let cc= counts i
           res <- threeCount (tail(x:xs)) cc
           return res
       else do
           res <- threeCount (tail(x:xs)) i
           return res