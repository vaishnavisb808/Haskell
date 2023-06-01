sayHello ::String -> IO()
sayHello x= putStrLn ("Hello "++ x ++ "!")

main = do
   fname <- getLine 
   lname <-getLine 
   sayHello (fname ++ lname)


