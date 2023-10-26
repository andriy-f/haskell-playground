module Basics where

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

mul2 :: Num a => a -> a
mul2 a = a * 2

add1 :: Num a => a -> a
add1 a = a + 1

multiply3 :: Num a => a -> a -> a -> a
multiply3 a b c = a * b * c

isLessThan :: Ord a => a -> a -> Bool
isLessThan x a = a < x
