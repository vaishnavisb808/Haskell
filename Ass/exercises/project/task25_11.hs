module Task2 where
import Data.Map as M
import Control.Applicative
--import GHC.CmmToAsm.PPC.Instr (Instr(XORIS))
conversionMap = M.fromList (zip [0..9] ['a'..'z'])
task x |not(Prelude.null x) = M.lookup (head x)conversionMap:
    if(Prelude.null x) then []
    else task (tail x)
    |otherwise = []
task2 :: [Int]->[Char]
task2 x = do
let list = task x
if (elem Nothing list) == True then "INVALID"
else do
let output=Prelude.map(\(Just x)->x)list
output

