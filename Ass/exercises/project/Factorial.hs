module Factorial where

factorial' ::Int->Int
factorial' n|n==0=1
            |n < 0 = error "fact only valid for non-negative integers"
            |n<=25 = n*factorial'(n-1)
            |n>25=error"overflow"