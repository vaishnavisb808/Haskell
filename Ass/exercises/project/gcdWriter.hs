import Control.Monad.Writer  
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }


instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  
gcd' :: Int -> Int -> Writer (DiffList String) Int  
gcd' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcd' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result  

