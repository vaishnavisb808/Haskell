--newtype Identity a = Identity a
instance Functor Identity where
fmap id a  = a
