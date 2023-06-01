module FunctionSpec where

import Data.Time
import Test.HUnit
import VisionApi.Logger.Internal
import VisionApi.Logger.Types


testcases ::IO Test
testcases = do
    time <- getCurrentTime
    let test1 = TestCase(assertEqual "basic test" (show time ++ " [INFO] InfoMessage\n") (logFormat time INFO "InfoMessage" ))
    let test2 = TestCase(assertEqual "empty message" (show time ++ " [DEBUG] \n") (logFormat time DEBUG ""  ))
    let testsuite = TestList [TestLabel "test1" test1,TestLabel "test2" test2]
    return testsuite

