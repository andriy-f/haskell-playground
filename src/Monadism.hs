module Monadism where

import Control.Monad (Monad (return, (>>=)), MonadPlus (mzero))
import Control.Monad.Writer (Writer, runWriter, tell, writer)
import Data.Monoid (Monoid (mappend, mempty))

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

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
      ]
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

knightIn3 :: KnightPos -> [KnightPos]
knightIn3 start = [start] >>= moveKnight >>= moveKnight >>= moveKnight

canKnightIn3 :: KnightPos -> KnightPos -> Bool
canKnightIn3 start end = end `elem` knightIn3 start

newtype MyWriter w a = MyWriter {runMyWriter :: (a, w)}

instance Functor (MyWriter w) where
  fmap f (MyWriter (x, v)) = MyWriter (f x, v)

instance (Monoid w) => Applicative (MyWriter w) where
  pure x = MyWriter (x, mempty)
  (MyWriter (f, v)) <*> (MyWriter (x, v')) = MyWriter (f x, v `mappend` v')

instance (Monoid w) => Monad (MyWriter w) where
  return = pure
  (MyWriter (x, v)) >>= f =
    let (MyWriter (y, v')) = f x
     in MyWriter (y, v `mappend` v')

myWriterUsage :: (Int, String)
myWriterUsage = runMyWriter $ do
  x <- MyWriter (1, "hello")
  y <- MyWriter (2, " world")
  return (x + y)

myWriterUsage' :: (Int, String)
myWriterUsage' = runMyWriter $ MyWriter (1, "hello") >>= (\x -> MyWriter (2, " world") >>= (\y -> return (x + y)))

getSomeString :: String
getSomeString = "This is some string"

getLengthWithLog :: String -> Writer [String] Int
getLengthWithLog s = writer (length s, ["Counted the characters in the string"])

evaluateStrLenghtWithLog :: Int -> Writer [String] String
evaluateStrLenghtWithLog n = do
  let evaluation = if n > 10 then "String is long" else "String is short"
  writer (evaluation, ["Evaluated the length of the string."])

theirWriterUsage = runWriter $ do
  tell ["Before got the length of the string"]
  len <- getLengthWithLog getSomeString
  tell ["After got the length of the string"]
  evaluateStrLenghtWithLog len

theirWriterUsage' = runWriter $ getLengthWithLog getSomeString >>= evaluateStrLenghtWithLog

functionAsApplicativeFunctorExample = (+) <$> (* 2) <*> (+ 10)

-- (->) r is Monad as well as functor and applicative functor
functionAsMonadExample = do
  x <- (* 2)
  y <- (+ 10)
  return (x + y)

-- ... function monad is called reader monad
functionAsRegularExample x =
  let a = (* 2) x
      b = (+ 10) x
   in a + b
