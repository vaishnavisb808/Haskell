import Test.QuickCheck
functorIdentity :: (Functor f, Eq (f a))=> f a ->Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f ) =>(a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x=(fmap g (fmap f x)) == (fmap (g.f) x)

{-can also write like this- functorCompose :: (Eq (f c), Functor f ) =>
                            (a -> b) 
                         -> (b -> c) 
                         -> f a 
                         -> Bool -}
f :: [Int] -> Bool
f x =functorIdentity x
c= functorCompose (+2) (*3)
li x = c (x:: [Int])

main ::IO()
main = quickCheck f






  {-RUN LIKE THIS when function f is defined outside,first want to import Test.QuickCheck 
ghci> :l FunctorProperty.hs      
[1 of 1] Compiling Main             ( FunctorProperty.hs, interpreted )
Ok, one module loaded.
ghci> :{                         
| let f ::[Int] -> Bool      
|     f x = functorIdentity x
| :}                         
ghci> quickCheck f
+++ OK, passed 100 tests.
ghci>-}
{- it after defining f and li then also main, we can call f and li in main then :l FunctorProperty.hs 
[1 of 1] Compiling Main             ( FunctorProperty.hs, interpreted )
Ok, one module loaded.
ghci> main
+++ OK, passed 100 tests.-}