isodd :: Int ->IO()
isodd x
        | rem x 2/=0 = putStrLn "even"
		| otherwise =putStrLn "odd"
		