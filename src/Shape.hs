module Shape where

class Shape a where
  area :: a -> Float

-- x y radius
data Circle = Circle Float Float Float

-- x1 y1 width height
data Rectangle = Rectangle Float Float Float Float

instance Shape Circle where
  area (Circle _ _ r) = pi * r ^ 2

instance Shape Rectangle where
  area (Rectangle _ _ w h) = w * h
