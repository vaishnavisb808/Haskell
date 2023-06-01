signum2 :: (Ord a, Num a) => a -> Int
signum2 x | x <  0     = -1
         | x == 0     = 0
         | otherwise  = 1