myAction :: IO String
myAction =(++) <$> getLine <*> getLine

myAction' :: IO String 
myAction' = do
    a<- getLine 
    b<- getLine 
    return $a++b