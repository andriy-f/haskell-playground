module Monadism where

import Control.Monad (MonadPlus (mzero))

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

listOfTuples'' :: [(Int, Char)]
listOfTuples'' = [(n, ch) | n <- [1, 2], ch <- ['a', 'b']]

listOfTuples' :: [(Int, Char)]
listOfTuples' = [1, 2] >>= (\n -> ['a', 'b'] >>= (\c -> return (n, c)))

-- MondaPlus is a typeclass for monads that can also act as monoids
-- mzero is the identity element of the monoid
guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

filterNumbersContaining7 :: [Int] -> [Int]
filterNumbersContaining7 xs = xs >>= (\x -> guard ('7' `elem` show x) >> return x)

filterNumbersContaining7' :: [Int] -> [Int]
filterNumbersContaining7' xs = do
  x <- xs
  guard ('7' `elem` show x)
  return x
