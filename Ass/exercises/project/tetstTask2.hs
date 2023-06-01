module EvaluateTask2 where


import Task2
import Test.HUnit


test1= TestCase (assertEqual "basic test" "abdf" (task2 [0,1,3,5]))

test2 = TestCase (assertEqual "empty input" "" (task2 []))

test3 =  TestCase (assertEqual "with invalid entry" "Invalid Input" (task2 [1,2,10]))


task2_tests = TestList[
	TestLabel "test1" test1,
	TestLabel "test2" test2,
	TestLabel "test3" test3]
main :: IO ()
main = do
    counts <- runTestTT ( test [
        test1,test2,
        test3
        ])
    print counts