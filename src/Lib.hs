module Lib
    ( someFunc
    , myAdd
    , qsort
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

someFunc :: IO ()
someFunc =
    putStrLn
        $  "myAdd: 3 + 4 = "
        ++ show (myAdd 3 4)
        ++ "\n"
        ++ let arr2sort = [10, 2, 5, 3, 1, 6, 7, 4, 2, 3, 4, 8, 9]
           in  "Qsort " ++ show arr2sort ++ ": " ++ show (qsort arr2sort)
