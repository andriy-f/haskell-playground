module BinaryTreeModule (BinaryTree (..), singleton, treeInsert) where

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
