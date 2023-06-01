module BintreeM where
data BinaryTree a =Leaf
                    | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Ord, Show)
   
root :: a -> BinaryTree a  
root x = Node x Leaf Leaf 
   
insert :: Ord  a => a-> BinaryTree  a -> BinaryTree a
insert x Leaf = root x 
insert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (insert x left) right
  | x > a = Node a left (insert x right)

isintree :: Ord a => a -> BinaryTree a -> Bool
isintree x Leaf = False
isintree x (Node a left right)
  | x == a = True
  | x > a = isintree x right
  | x < a = isintree x left
  
instance Functor BinaryTree where
  fmap f Leaf = Leaf
  fmap f (Node x lsubtree rsubtree) =
     Node (f x) (fmap f lsubtree) (fmap f rsubtree) 