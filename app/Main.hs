module Main where

import PlayTypes (Vector3D (..), vPlus)

main :: IO ()
main =
  let -- v1 = MyVector2D 1 2
      v1 = Vector3D 1 2 3
      v2 = Vector3D 3 2 1
   in -- putStrLn "This is Haskell playground.\nTo run tests, enter `stack test`" ++
      putStrLn ("Vector plus" ++ show (vPlus v1 v2))
