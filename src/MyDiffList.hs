module MyDiffList (MyDiffList, toMyDiffList, fromMyDiffList) where

import Data.Monoid (Monoid (mappend, mempty))

-- DiffList is analogy of List but with always efficient appending

newtype MyDiffList a = MyDiffList {getMyDiffList :: [a] -> [a]}

toMyDiffList :: [a] -> MyDiffList a
toMyDiffList xs = MyDiffList (xs ++)

fromMyDiffList :: MyDiffList a -> [a]
fromMyDiffList (MyDiffList f) = f []

instance Semigroup (MyDiffList a) where
  (MyDiffList f) <> (MyDiffList g) = MyDiffList {getMyDiffList = f . g}

instance Monoid (MyDiffList a) where
  mempty = MyDiffList ([] ++)
