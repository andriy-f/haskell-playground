module MyDiffList
  ( MyDiffList,
    toMyDiffList,
    fromMyDiffList,
    countWithDiffList,
    countWithoutDiffList,
    runCountWithDiffList,
    runCountWithoutDiffList,
  )
where

import Control.Monad.Writer (Writer, runWriter, tell)
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

countWithoutDiffList :: Int -> Writer ([String]) ()
countWithoutDiffList 0 = do
  tell ["0"]
countWithoutDiffList n = do
  countWithoutDiffList (n - 1)
  tell [show n]

-- For performance showcase (slower)
runCountWithoutDiffList = mapM_ putStrLn . snd . runWriter $ countWithoutDiffList 500000

countWithDiffList :: Int -> Writer (MyDiffList String) ()
countWithDiffList 0 = do
  tell $ toMyDiffList ["0"]
countWithDiffList n = do
  countWithDiffList (n - 1)
  tell $ toMyDiffList [show (n - 1)]

-- For performance showcase (faster)
runCountWithDiffList = mapM_ putStrLn . fromMyDiffList . snd . runWriter $ countWithDiffList 500000
