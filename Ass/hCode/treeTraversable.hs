import  Data.Foldable
data BinaryTree a =
    Leaf 
  | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)
    
root :: a -> BinaryTree a
root x = Node Leaf x Leaf

insert :: Ord a
  => a
  -> BinaryTree a
  -> BinaryTree a  
  
insert x Leaf = root x -- base case
insert x (Node left a right)
  | x == a = Node left a right
  | x < a = Node (insert x left) a right
  | x > a = Node left a (insert x right)
  
isintree :: (Ord a)
  => a
  -> BinaryTree a
  -> Bool
  
isintree x Leaf = False
isintree x (Node left a right)
  | x == a = True
  | x > a = isintree x right
  | x < a = isintree x left
  
instance Functor BinaryTree where
  fmap f Leaf = Leaf
  fmap f (Node lsubtree x rsubtree)
    = Node (fmap f lsubtree) (f x) (fmap f rsubtree)
    
instance Foldable BinaryTree where
  foldMap f Leaf = mempty
  foldMap f (Node left x right) =
    foldMap f left `mappend` f x `mappend` foldMap f right
  foldr f acc Leaf = acc
  foldr f acc (Node left x right) =
    foldr f (f x (foldr f acc right)) left
    
inorder Leaf = []
inorder (Node left x right) = inorder left ++ (x: inorder right)

preorder Leaf = []
preorder (Node left x right) =(x : preorder left) ++ preorder right

postorder Leaf = []

postorder (Node left x right) = postorder right ++ (x : postorder left)

instance Traversable BinaryTree where
    traverse f Leaf = pure Leaf
    traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r 








