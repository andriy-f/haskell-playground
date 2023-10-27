module GenericShape where

-- class (Floating t) => Shape a where
--   area :: a -> t

-- use most precise Floating number for area
class Shape a where
  area :: a -> Double

data Circle a = Circle a a a
-- data Circle a = Circle {x1 :: a, y1 :: a, radius :: a}

-- instance (Floating t) => Shape (Circle t) where
  -- area (Circle _ _ r) = pi * r ^ 2

anArea :: (Floating t) => Circle t -> t
anArea (Circle _ _ r) = pi * r ^ 2
