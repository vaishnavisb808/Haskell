showy :: Integer -> IO()
y=20
showy y=
      putStrLn(" y is"++ show y)
showy :: IO()
y=6
showy=
      putStrLn(" y is"++ show y)
bindExp y =
   let x=10; y=5 in
    "Value Y is "++show y
     ++ " value X is "++show x	