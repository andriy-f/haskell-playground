module Main where

import PlayTypes (Vector3D (..), vPlus)

main :: IO ()
main = vectorAddingIO

vectorAddingIO = do
  putStrLn "Enter 3 Numbers for 3D Vector #1:"
  x1 <- getLine
  y1 <- getLine
  z1 <- getLine
  putStrLn "Enter 3 Numbers for 3D Vector #2:"
  x2 <- getLine
  y2 <- getLine
  z2 <- getLine
  putStrLn "Result of adding those vectors:"
  let v1 = Vector3D (read x1) (read y1) (read z1)
      v2 = Vector3D (read x2) (read y2) (read z2)
   in putStrLn (show (vPlus v1 v2))
