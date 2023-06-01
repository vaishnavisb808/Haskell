
import ApiSpec
import FunctionSpec
import Test.HUnit

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

main :: IO ()
main = do
    test<- testcases
    runTestTT test
    hspec businessLogicSpec 
