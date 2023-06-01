 
arithmetic :: (RealFrac a, Integral b) => a -> a -> b
arithmetic x y = truncate ((x+y)/2)