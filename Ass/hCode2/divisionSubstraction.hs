divi :: Integer -> Integer-> Integer
divi a b| (b>a) = 0
        | (b-a)==0 =1
        | otherwise = 1+ divi (a-b) b