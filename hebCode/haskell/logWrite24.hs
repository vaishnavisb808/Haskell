isBing ::Int -> (Bool, String)
isBing x = (x > 9, "Compared size to 9")

applyLog :: (a,String) -> (a -> (b,String))->(b,String)
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)
mygcd :: [Char] -> [Char] -> [Char]
mygcd a b  
  | b == 0 = do [ "finished with " ++ show a] >>=return a
  | otherwise = do (show a ++ "mod" ++ show b ++ show (a `mod` b))