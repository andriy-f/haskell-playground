module Main where

import Control.Monad (when)
import PlayTypes (Vector3D (..), vPlus)

main :: IO ()
main = charReadWithPromptIO

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
   in print (vPlus v1 v2)

wordReversingIO = do
  putStrLn "Enter a sentence to reverse:"
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

charReadWithPromptIO = do
  putStrLn "Enter characters:"
  charReadIO

charReadIO = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    charReadIO
