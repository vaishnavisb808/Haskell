mySum :: [Int] ->Int
mySum[] = 0
mySum(x:xs) = x +mySum xs

mySum2 :: Int ->[Int ] ->Int 
mySum2 s [] = s
mySum2 s (x:xs) = let s' = s+ x in mySum2 s' xs
mySum2' :: [Int ] -> Int -> Int 
mySum2' [] s = s
mySum2' (x:xs)' (mySum2' xs s')