import System.IO
measurements :: (Ord a,Num a) => [a] -> [a]
measurements [] = error "Invalid Input"
measurements [x1,x2,x3] = []
measurements (x:xs) = do
        let num1 = x
        let num2 = head xs
        let num3 = head (tail xs)
        let num4 = head (tail (tail xs))
        let sum1 = num1 + num2 + num3
        let sum2 = num2 + num3 + num4
        if ((sum2 - sum1) > 0) then do
            let newList = 1 : measurements xs
            newList
        else
         (0:measurements xs)
countNumber :: (Ord a, Num a) => [a] -> Int
countNumber input = do
        let only1 = measurements input
        let len = length $ filter (== 1) only1
        len
main :: IO()
main = do
    inp <- fmap lines (readFile "input.txt")   
    let inpList = map (read :: String -> Int) inp
    let finalOutput = countNumber inpList
    print finalOutput





