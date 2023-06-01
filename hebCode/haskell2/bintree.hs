module Bintree where
data BinaryTree a =Leaf
                    | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Ord, Show)
   
root :: a -> BinaryTree a  
root x = Node x Leaf Leaf 
   
insert :: Ord  a => a
        -> BinaryTree  a
        -> BinaryTree a

insert x Leaf = root x 
insert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (insert x left) right
  | x > a = Node a left (insert x right)

nums = [8,6,4,1, 7, 3, 5]
numtree = foldr insert Leaf nums  

isintree :: Ord a 
  => a
  -> BinaryTree a 
  -> Bool
  
isintree x Leaf = False
isintree x (Node a left right)
  | x == a = True
  | x > a = isintree x right
  | x < a = isintree x left

{--class YesNo a where
  yesNo :: a -> Bool
  
instance YesNo (BinaryTree a) where
  yesNo Leaf = False
  yesNo _ = isintree $ BinaryTree _ --} 
  
instance Functor BinaryTree where
  fmap f Leaf = Leaf
  fmap f (Node x lsubtree rsubtree) =
     Node (f x) (fmap f lsubtree) (fmap f rsubtree) 
	 
data Inputinvalid = Equal deriving (Show, Eq)	

type ValidateIn a = Either Inputinvalid a
toString :: Inputinvalid -> String
toString Equal = "The number already exists"


inOkay x (Node a left right) = case x == a of
  True -> Left Equal
  False -> Right (insert x (Node a left right))
  
  