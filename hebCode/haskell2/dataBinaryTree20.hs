import Data.Foldable
--import Control.Monad.Error (Functor)
data BinaryTree a = Leaf
                   | Node a (BinaryTree a)  (BinaryTree a) deriving (Eq, Ord , Show)

root :: a -> BinaryTree a
root x = Node x Leaf Leaf
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = root x -- base case
insert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node x(insert x left ) right
  | x > a = Node x left (insert x right)
  --nums = [8,6,4,1,7,3,5]
  --numtree = foldr insert Leaf nums
isintree :: (Ord a)
    => a
    -> BinaryTree a
    -> Bool
isintree x Leaf = False 
isintree x (Node a left right)
    | x == a = True 
    | x > a = isintree x right
    | x< a = isintree x left

class YesNo a where
    yesNo :: a -> Bool   

instance YesNo (BinaryTree a) where
    yesNo Leaf = False 
    yesNo _ = True
instance Functor BinaryTree where 
    fmap f Leaf = Leaf
    fmap  f (Node x isubtree rsubtree) = Node (f x) (fmap f isubtree) (fmap f rsubtree)
--data BinaryTree a = Leaf
  --          | Node (BinaryTree a) a (BinaryTree a)

    
