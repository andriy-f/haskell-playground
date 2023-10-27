module GenericShape where

-- class (Floating t) => Shape a where
--   area :: a -> t

-- a - type constructor
-- (a t) - concrete type
-- t - concrete type
class Shape a where
  area :: (Floating t) => a t -> t
  perimeter :: (Floating t) => a t -> t

-- Circle is a type constructor
data Circle a = Circle a a a

-- Type constructor Circle is an instance of type class Shape
instance Shape Circle where
  area (Circle _ _ r) = pi * r ^ 2
  perimeter (Circle _ _ r) = 2 * pi * r
