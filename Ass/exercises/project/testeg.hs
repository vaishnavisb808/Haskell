import Test.HUnit as T
tests = test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),
                  "test2" ~: do (x, y) <- partA 3
                                assertEqual "for the first result of partA," 5 x
                                partB y @? "(partB " ++ show y ++ ") failed" ]
main :: IO ()
main = runTestTTAndExit tests
--tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]