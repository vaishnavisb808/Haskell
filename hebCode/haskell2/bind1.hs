bindExp :: Integer ->IO()
bindExp x= 
   let y=5 in
   putStrLn ("The integer was " ++ show x
   ++" and local one was "++ show y)
   