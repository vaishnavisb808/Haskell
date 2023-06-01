e :: IO Integer
e = let ioi = readIO "1"
 changed =(*3) (fmap ("123"++) .show ioi)
