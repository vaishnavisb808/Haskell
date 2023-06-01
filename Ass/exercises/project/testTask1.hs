module EvaluateTask1 where


import Task1 
import Test.HUnit

test1 = TestCase(assertEqual "basic test" ("bc",1) (task1 'a' "abc"))

test2 = TestCase(assertEqual "multiple occurences" ("acc",4) (task1 'b' "babccbb"))

test3 = TestCase(assertEqual "single char string" ("",1) (task1 'a' "a"))

test4 = TestCase(assertEqual "no occurences" ("bbbb",0) (task1 'a' "bbbb"))

task1_tests = TestList [
    TestLabel "test1" test1, 
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4]


main :: IO ()
main = do
    counts <- runTestTT ( test [
        test1,test2,
        test3,test4
        ])
    print counts