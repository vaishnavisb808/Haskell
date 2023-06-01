module FactTest where
import Factorial as F
import Test.HUnit
import System.Exit

test0=TestCase (assertEqual "factorial 1"(1)(F.factorial' 0))
test1=TestCase (assertEqual"factorial of 5"(120)(F.factorial' 5))
test2=TestCase (assertEqual"factorial negative"(-9)(F.factorial' (-1)))
test3=TestCase (assertEqual"factorial overflow"(199999999999)(F.factorial' 27))
--tests=TestList [TestLabel "test1" test0,TestLabel "test2" test1]

 
main :: IO ()
main = do
    counts <- runTestTT ( test [
        test0,test1,
        test2,test3
        ])
    print counts
    
