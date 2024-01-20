module RopeWalking where

type Birds = Int

type Pole = (Birds, Birds)

-- pre-monad use example
landLeftV0 :: Birds -> Pole -> Pole
landLeftV0 n (left, right) = (left + n, right)

landRightV0 :: Birds -> Pole -> Pole
landRightV0 n (left, right) = (left, right + n)

-- >>= prototype
x -: f = f x

exampleBirdsflyV0 :: Pole
exampleBirdsflyV0 = (0, 0) -: landLeftV0 1 -: landRightV0 1 -: landLeftV0 2

-- Mondad use example
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) <= 3 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) <= 3 = Just (left, right + n)
  | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

exampleBirdfly :: Maybe Pole
exampleBirdfly = return (0, 0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
