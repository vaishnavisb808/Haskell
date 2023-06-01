
data Color = Red |
             Yellow |
             Blue |
             Green |
             Purple |
             Orange |
             Brown deriving (Eq,Show)
instance Semigroup Color where 
    (<>) Transparent c = c
    (<>) c Transparent = c
    (<>) Red Blue=Purple
    (<>) Blue Red =Purple
    (<>) Yellow Blue = Green
    (<>)  Blue Yellow = Green
    (<>)  Red Yellow = Green
    (<>)  Yellow Red = Green
    (<>) a b | a == b = a
              | all(`elem` [Red,Blue,Purple])[a ,b] = Purple
              |otherwise = Brown
instance Monoid Color where
    mempty = Transparent
    mappend = (<>)