module Heathrow2London (getHeathrow2LondonMinPathLength) where

import Data.List (foldl1', minimumBy)

{-

Heathrow to London

Find min path length from Heathrow to London

Depiction:

A---50---A1---5---A2--40---A3--10---A4
         |        |        |        |
         30       20       25       0
         |        |        |        |
B---10---B1---90--B2---2---B3---8---B4

Thinking process:

minPathLengthH2L = min(minPathLengthTo(A3) + 10, minPathLengthTo(B3) + 8)

minPathLengthTo(A3) = min(minPathLengthTo(A2)+40, minPathLengthTo(B2)+2+25)
minPathLengthTo(B3) = min(minPathLengthTo(B2)+2, minPathLengthTo(A2)+40+25)

minPathLengthTo(A2) = min(minPathLengthTo(A1)+5, minPathLengthTo(B1)+90+20)
minPathLengthTo(B2) = min(minPathLengthTo(B1)+90, minPathLengthTo(A1)+5+20)

minPathLengthTo(A1) = min(50, 10+30)
minPathLengthTo(B1) = min(10, 50+30)

Manual calculation:

minPathLengthTo(A1) = min(50, 10+30) = min(50, 40) = 40
minPathLengthTo(B1) = min(10, 50+30) = min(10, 80) = 10

minPathLengthTo(A2) = min(minPathLengthTo(A1)+5, minPathLengthTo(B1)+90+20) = min(40+5, 10+90+20)
  = min(45, 120) = 45
minPathLengthTo(B2) = min(minPathLengthTo(B1)+90, minPathLengthTo(A1)+5+20) = min(10+90, 40+5+20)
  = min(100, 65) = 65

minPathLengthTo(A3) = min(minPathLengthTo(A2)+40, minPathLengthTo(B2)+2+25) = min(45+40, 65+2+25)
  = min(85, 92) = 85
minPathLengthTo(B3) = min(minPathLengthTo(B2)+2, minPathLengthTo(A2)+40+25) = min(65+2, 45+40+25)
  = min(67, 110) = 67

minPathLengthH2L = min(minPathLengthTo(A3) + 10, minPathLengthTo(B3) + 8) = min(85+10, 67+8)
  = min(95, 75) = 75

-}
type Section = Int

commasToSpacesFun :: Char -> Char
commasToSpacesFun ',' = ' '
commasToSpacesFun c = c

commasToSpaces :: String -> String
commasToSpaces = map commasToSpacesFun

getDefaultHeathrowToLondonPathsAsString :: String
getDefaultHeathrowToLondonPathsAsString = "50,10,30,5,90,20,40,2,25,10,8,0"

findMinPathLength :: Section -> Int
findMinPathLength _ = 0 -- TODO
  where
    paths = getDefaultHeathrowToLondonPathsAsString
    pathsArr = map read $ words $ commasToSpaces paths :: [Int]

-- input: [50,10,30,5,90,20,40,2,25,10,8,0]
-- output: [(50,10,30),(5,90,20),(40,2,25),(10,8,0)]
makeTriplets :: [a] -> [(a, a, a)]
makeTriplets [] = []
makeTriplets (x : y : z : xs) = (x, y, z) : makeTriplets xs

-- Aprev --- aEdgeLength --- Anext
--                             |
--                            AnextBnextEdgeLength
--                             |
-- Bprev --- bEdgeLength --- Bnext
--
-- Input: (minPathToAprev, minPathToBprev) (AEdgeLength, bEdgeLength, AnextBnextEdgeLength)
calcNextABMinPathLength :: (Int, Int) -> (Int, Int, Int) -> (Int, Int)
calcNextABMinPathLength (minPathToAprev, minPathToBprev) (aEdgeLength, bEdgeLength, anextBnextEdgeLength) =
  (minPathToAnext, minPathToBnext)
  where
    minPathToAnext = min (minPathToAprev + aEdgeLength) (minPathToBprev + bEdgeLength + anextBnextEdgeLength)
    minPathToBnext = min (minPathToBprev + bEdgeLength) (minPathToAprev + aEdgeLength + anextBnextEdgeLength)

-- Sample input: "50,10,30,5,90,20,40,2,25,10,8,0"
getHeathrow2LondonMinPathLength :: String -> Int
getHeathrow2LondonMinPathLength paths = minPathLength
  where
    pathsAsArr = map read $ words $ commasToSpaces paths :: [Int]
    pathsAsTriplets = makeTriplets pathsAsArr
    minPathLengthCalc = foldl calcNextABMinPathLength (0, 0) pathsAsTriplets
    (aLastMinPathLength, bLastMinPathLength) = minPathLengthCalc
    minPathLength = min aLastMinPathLength bLastMinPathLength
