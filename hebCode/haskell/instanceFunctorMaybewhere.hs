instance Functor Maybe where
    fmap func (Just n)=Just (func n)
    fmap func Nothing = Nothing 