module Randomization (main) where

import System.Random (Random, RandomGen, mkStdGen, getStdGen,
  random, randoms, randomR, randomRs)

main = do
  putStrLn "Welcome to randomization app"
  putStrLn "Enter number of randoms to generate:"
  numberStr <- getLine
  let number = read numberStr :: Int
      myRandomNumbers = take number (getRandoms (mkStdGen 42) :: [Int])

  putStrLn ("Your randoms: " ++ show myRandomNumbers)

-- get infinite list of randoms [a] from a seed g
getRandoms :: (Random a, RandomGen g) => g -> [a]
getRandoms g =
  let (r, gNew) = random g
   in r : getRandoms gNew

getFiniteRandoms :: (Random a, RandomGen g) => Int -> g -> ([a], g)
getFiniteRandoms 0 g = ([], g)
getFiniteRandoms n g =
  let (r, gNew) = random g
      (randomsNminus1, gFinal) = getFiniteRandoms (n - 1) gNew
   in (r : randomsNminus1, gFinal)

generatePassword :: Int -> String
generatePassword n =
  let (randoms, _) = getFiniteRandoms n (mkStdGen 42)
   in map toEnum randoms
