module MyFunctor where

import BinaryTreeModule (BinaryTree (..))
import qualified Data.Map as Map

-- f is not concrete type but type constructor
-- (a -> b) is a function
-- (f a), (f b) are concrete types
class MyFunctor f where
  myFMap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
  myFMap f (Just x) = Just (f x)
  myFMap f Nothing = Nothing

-- Here [] is a type constructor for lists
instance MyFunctor [] where
  myFMap = map

instance MyFunctor BinaryTree where
  myFMap f EmptyTree = EmptyTree
  myFMap f (Node x left right) = Node (f x) (myFMap f left) (myFMap f right)

instance MyFunctor (Either a) where
  myFMap f (Right x) = Right (f x)
  myFMap f (Left x) = Left x

instance MyFunctor (Map.Map k) where
  myFMap = Map.map

instance MyFunctor IO where
  myFMap f action = do
    result <- action
    return (f result)
