
import ExeTwo
import Test.HUnit
u1=User "aa" 1
u2=User "bb" 3
u3=User "cc" 1

test1=TestCase (assertEqual "simple check" [u1,u2](task1 [u1,u2,u3]))
test2=TestCase(assertFailure  "invalid")
test3=TestCase (assertEqual "order check" [u3,u2](task1 [u2,u3]))
test4=TestCase (assertEqual "error check" [u1,u3](task1 [u1,u2,u3]))
tests=TestList [TestLabel "first" test1 ,TestLabel "invalid" test2,TestLabel "order" test3,TestLabel "error" test4]
main= runTestTT tests