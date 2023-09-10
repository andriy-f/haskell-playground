module Lib
    ( myAdd
    , add1
    , qsort
    , muln
    , pairs
    , curryTest
    )
where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) =
    let firstPartSorted  = qsort [ y | y <- xs, y <= x ]
        secondPartSorted = qsort [ y | y <- xs, y > x ]
    in  firstPartSorted ++ x : secondPartSorted

add1 a = 1 + a

myAdd :: Num a => a -> a -> a
myAdd a b = a + b

-- Return array or elements from another array multiplied by n
-- Sample of using foldr on infinite list
-- Warn: if source list in infinite, result is also infinite, so use take or something
muln :: Num a => a -> [a] -> [a]
muln n = foldr (\x acc -> x * n : acc) []

-- not something that was intended
pairs :: (Num a) => (a -> a -> a) -> [a] -> [a]
pairs f t = t
-- pairs f (x:y:t) = f x y : pairs f t

curryTest :: (Show b) => Char -> b -> [Char]
curryTest a b = a : show b
