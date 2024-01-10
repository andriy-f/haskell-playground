module Randomization (main) where

import System.Random
  ( Random,
    RandomGen,
    getStdGen,
    mkStdGen,
    random,
    randomR,
    randomRs,
    randoms,
  )

main = do
  putStrLn "Welcome to randomization app"
  putStrLn "Enter lenght of pass to generate:"
  numberStr <- getLine
  let number = read numberStr :: Int
  res <- generatePassword number ('a', '0')

  putStrLn ("Result: " ++ show res)

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

generatePassword :: Int -> (Char, Char) -> IO String
generatePassword n (min, max) = do
  gen <- getStdGen
  return (take n $ randomRs (min, max) gen)
