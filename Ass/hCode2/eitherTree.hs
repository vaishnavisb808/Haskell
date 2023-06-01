{-
treeB :: (Ord a) => a -> Either root a
treeB x = case x == a of {
  True -> Right  ("root is "++ root)
  x > a -> Right ("node is "++ a)
  x < a -> Right ("node is "++ a)
  False -> Left Leaf }
 data Inputinvalid = Equal deriving (Show, Eq)	
-}
import Bintree
type ValidateIn a = Either Inputinvalid a
toString :: Inputinvalid -> String
toString Equal = "The number already exists"



inOkay x (Node a left right) = case x == a of
  True -> Left Equal
  False -> Right (insert x (Node a left right))
