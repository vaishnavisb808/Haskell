newtype State s a =State {runState :: s -> (a,s)}

newtype Reader r a = Reader {runReader ::r ->a}
mkStdGen :: Int -> stdGen
next ::g -> (Int,g)
random :: RandomGen 

-- on prompt - postinncrement = do {x<- get ; put (x+1);return x}
-- then run this ghci> runState postinncrement 1
--get this(1,2)