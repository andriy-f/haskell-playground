module Monadism where

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

listOfTuples'' :: [(Int, Char)]
listOfTuples'' = [(n, ch) | n <- [1, 2], ch <- ['a', 'b']]

listOfTuples' :: [(Int, Char)]
listOfTuples' = [1, 2] >>= (\n -> ['a', 'b'] >>= (\c -> return (n, c)))
