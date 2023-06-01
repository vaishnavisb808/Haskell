main= do 
   fname <- getLine 
   lname <-getLine 
   sayHello (fname ++ lname)