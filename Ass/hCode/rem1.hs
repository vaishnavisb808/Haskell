findOdd ::Int->IO()

findOdd x =
   if rem x 2==0
     then putStrLn"even"
   else  putStrLn"odd"
      