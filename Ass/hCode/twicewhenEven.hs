twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

text :: [Integer ] ->[Integer ]
text xs = do
  x <- xs
  [x * 2]

textPrint :: IO()
textPrint = (putStrLn "String") >> putStrLn "ex"

maybePrint:: [Int] ->[Maybe Int]
maybePrint xs >>= xs ((:Just ).(*2))