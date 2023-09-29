module Basics where

mul2 :: Num a => a -> a
mul2 a = a * 2

add1 :: Num a => a -> a
add1 a = a + 1

multiply3 :: Num a => a -> a -> a -> a
multiply3 a b c = a * b * c

isLessThan :: Ord a => a -> a -> Bool
isLessThan x a = a < x
