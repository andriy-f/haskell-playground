module BinaryTreeModule (BinaryTree (..), singleton, treeInsert, treeElem) where

import qualified Data.Foldable as F

data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a) deriving (Show, Read, Eq)

-- Single-element tree
singleton :: a -> BinaryTree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> BinaryTree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

instance F.Foldable BinaryTree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x left right) =
    F.foldMap f left
      `mappend` f x
      `mappend` F.foldMap f right
