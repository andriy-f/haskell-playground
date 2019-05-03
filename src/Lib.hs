module Lib
    ( someFunc
    , myAdd
    , qsort
    , muln
    )
where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) =
    let firstPartSorted  = qsort [ y | y <- xs, y <= x ]
        secondPartSorted = qsort [ y | y <- xs, y > x ]
    in  firstPartSorted ++ x : secondPartSorted

myAdd :: Num a => a -> a -> a
myAdd a b = a + b

-- Return array or elements from another array multiplied by n
-- Sample of using foldr on infinite list
-- Warn: if source list in infinite, result is also infinite, so use take or something
muln :: Num a => a -> [a] -> [a]
muln n arr = foldr (\x acc -> x * n : acc) [] arr

someFunc :: IO ()
someFunc = putStrLn $ "infinifold" ++ show (take 12 $ muln 2 [1 ..])
