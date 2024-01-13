module Heathrow2London where

import Data.List (foldl1', minimumBy)
{-
  A---50---A1---5---A2--40---A3--10---A4
           |        |        |        |
           30       20       25       0
           |        |        |        |
  B---10---B1---90--B2---2---B3---8---B4

  minPathLenghTo(A4) = minPathLenghTo(B4) = min(minPathLenghTo(A3)+10, minPathLenghTo(B3) + 8)
  minPathLenghTo(A3) = min(minPathLenghTo(A2)+40, minPathLenghTo(B3)+25)
  minPathLenghTo(B3) = min(minPathLenghTo(B2)+2, minPathLenghTo(A3)+25)
-}
type Section = Int
findMinPathLength :: Section -> Int
findMinPathLength _ = 0 -- TODO
  where
    paths = "50,10,30,5,90,20,40,2,25,10,8,0"
    pathsArr = map read $ words $ map (\c -> if c == ',' then ' ' else c) paths :: [Int]
