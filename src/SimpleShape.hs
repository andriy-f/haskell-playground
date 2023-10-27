module SimpleShape (Shape (..), area) where

-- Circle x y radius
-- Rectangle x1 y1 width height
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle _ _ w h) = w * h
