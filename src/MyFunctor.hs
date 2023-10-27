module MyFunctor where
import BinaryTreeModule (BinaryTree(..))

-- f is not concrete type but type constructor
-- (a -> b) is a function
-- (f a), (f b) are concrete types
class MyFunctor f where
  fMap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
  fMap f (Just x) = Just (f x)
  fMap f Nothing = Nothing

-- Here [] is a type constructor for lists
instance MyFunctor [] where
  fMap = map

instance MyFunctor BinaryTree where
  fMap f EmptyTree = EmptyTree
  fMap f (Node x left right) = Node (f x) (fMap f left) (fMap f right)
