signum1 :: (Ord a, Num a) => a -> Int
signum1 x | x <  0  = -1
         | x == 0  = 0
         | x >  0  = 1  | x > 0 = 1
          | x == 0 =0