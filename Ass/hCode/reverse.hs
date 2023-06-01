main:: IO()
main = do
    putStrLn "enter the String"
    x <- getLine 
    y :: [String]
    y <- words x
    z :: [String]
    z <- reverse y
    foldNew :: String
    foldNew <- foldr(++) ("") $ head z : (map (\x -> " " ++ x) $ tail z)
