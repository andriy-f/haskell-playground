module Modules where

findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k, v) : xs) =
  if key == k
    then Just v
    else findKey key xs

-- This is a textbook recursive function that operates on a list
findKey2 :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey2 key = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing
