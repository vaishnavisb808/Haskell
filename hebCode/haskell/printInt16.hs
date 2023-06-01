printString ::Maybe String -> IO()
printString Nothing = putStrLn "value missing"
printString (Just val) = putStrLn val


printInt :: Maybe Int -> String
printInt Nothing = show "value is missing"
printInt (Just val)= show val