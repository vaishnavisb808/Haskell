bindExp :: Integer ->IO()
bindExp x= 
   let y=5 in
   let z= y+x in
   
   putStrLn ("The integer was " ++ show x
   ++" and local one was "++ show y ++ " summation is " ++ show z)
   