import Control.Monad.Reader

main = putStrLn $runReader isInRange 25 

isInRange :: Reader Int String
isInRange = do
                a<-isGreaterThan5
                b<-isLessThan50
                return $a ++ " and " ++ b

isGreaterThan5 :: Reader Int String
isGreaterThan5 = do
  y <- ask
  if y > 5
    then return $show y ++ " is greater than 5"
    else return $show y ++ " is less than 5"

isLessThan50 :: Reader Int String
isLessThan50 = do
  z <- ask
  if z < 50
    then return $show z ++ " is less than 50"
    else return $show z ++ " is greater than 50"