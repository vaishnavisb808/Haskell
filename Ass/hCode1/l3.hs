l3rd::String ->Char
l3rd x= x!!2

lIndex :: Int ->Char
lIndex x = "curry is awesome" !! x

rvrs :: String ->String
rvrs x = drop 9 x ++ " "++ drop 5 (take 8 x) ++" " ++ take 5 x