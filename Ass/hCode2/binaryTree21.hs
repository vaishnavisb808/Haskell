import Data.Bool (bool)
import Data.Foldable
import Control.Monad.Error (Functor)
data BinaryTree a = Leaf
                   | Node  (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord , Show)
                   
root :: a -> BinaryTree a
root x = Node Leaf x Leaf
insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert x Leaf = root x -- base case
insert x (Node left a right)
  | x == a = Node left x right
  | x < a = Node (insert x left )a right
  | x > a = Node  left a (insert x right)
  --nums = [8,6,4,1,7,3,5]
  --numtree = foldr insert Leaf nums
isintree :: Ord a=> a -> BinaryTree a-> Bool
isintree x Leaf = False
isintree x (Node left a right)
  | x == a = True
  | x > a = isintree x right
  | x < a = isintree x left

{-class YesNo a where
    yesNo :: a -> Bool   

instance YesNo (BinaryTree a) where
    yesNo Leaf = False 
    yesNo _ = True-}
instance Functor BinaryTree where
  fmap f Leaf = Leaf
  fmap f (Node lsubtree x rsubtree)= Node (fmap f lsubtree) (f x) (fmap f rsubtree) 

instance Foldable BinaryTree where
   foldMap f Leaf = mempty
   foldMap f (Node left x right )= foldMap f left ` mappend ` f x ` mappend ` foldMap f right
   foldr f acc Leaf = acc
   foldr f acc (Node left x right )= foldr f (f x (foldr f acc right) ) left
inorder Leaf = []
inorder (Node left x right) = inorder left ++ (x: inorder right)
preorder Leaf= []
preorder (Node x left right) =preorder x ++ (left: preorder right)
postorder Leaf = []
postorder (Node left right x) = postorder left ++ (right : postorder x)