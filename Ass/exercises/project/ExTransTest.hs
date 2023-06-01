import Test.HUnit
import System.Exit
import ExTrans
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))

-- takeMaybeString::MaybeT IO String->Maybe String
takeMaybeString x = do
    s<-runMaybeT x
    s



    --return s
-- testAskforAge = TestCase(assertEqual "age" (Just"23") (takeMaybeString (askforAge "23")) )
--testAskforName=TestCase(assertEqual "name" (Just"vaish")( ExTrans.askforName "vaish"))
--testInvalid = TestCase(assertFailure  "invalid")

-- main :: IO ()
-- main = do
--     let tests = TestList[TestLabel "testAskforAge" testAskforAge]--,TestLabel "testInvalid" testInvalid,TestLabel "testAskforName" testAskforName]
--     counts <- runTestTT tests
--     if (errors counts + failures counts == 0)
--         then exitSuccess
--         else exitFailure