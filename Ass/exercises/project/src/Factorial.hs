module Factorial where
--Module for performe factorial function

factorial :: Int -> Maybe Int
factorial n
  | n < 0     = Nothing
  | otherwise = Just $ product [1..n]



--n==0=1 -- checking 0
  --          |n < 0 = error "fact only valid for non-negative integers"
    --        |n<=25 = n*factorial'(n-1)
      --      |n>25=error"overflow"