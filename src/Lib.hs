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
someFunc =
    putStrLn
        $  "myAdd: 3 + 4 = "
        ++ show (myAdd 3 4)
        ++ "\n"
        ++ let arr2sort = [10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9]
           in  "Qsort "
               ++ show arr2sort
               ++ ": "
               ++ show (qsort arr2sort)
               ++ "\n"
               ++ "infinifold"
               ++ show (take 12 $ muln 2 [1 ..])
