brokenFact1::Integer -> Integer
brokenFact1 n=n * brokenFact1(n-1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)