data Spice= Spice {
                  spice :: String,
                  quantity::Maybe Int,
                  price ::Int
}deriving Show

instance Functor Spice where
  fmap f Spice' = Spice {
    spice = f (spice Spice'),
    quantity = fmap f (quantity Spice'),
    price = f <$> (price Spice')
  }
  Spice' :: Spice (String, Int, Int)
Spice' = Spice
  ("Turmuric", 60, 15)