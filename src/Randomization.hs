module Randomization (main) where

import System.Random (Random, RandomGen, mkStdGen, random)

main = do
  putStrLn "Welcome to randomization app"
  putStrLn "Nter number of randoms to generate:"
  numberStr <- getLine
  let number = read numberStr :: Int
      myRandomMmers = take number (myRandoms (mkStdGen 42) :: [Int])

  putStrLn ("Your randoms: " ++ show myRandomMmers)

myRandoms :: (Random a, RandomGen g) => g -> [a]
myRandoms g =
  let (r, gNew) = random g
   in r : myRandoms gNew
