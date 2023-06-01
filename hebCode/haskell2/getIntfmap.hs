getInt ::IO Int
getInt = fmap read getLine
