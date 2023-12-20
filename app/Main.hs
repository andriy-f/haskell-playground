module Main where

import Control.Monad (forM, forever, mapM, when)
import Data.Char (toUpper)
import PlayTypes (Vector3D (..), vPlus)
import Text.ParserCombinators.ReadP (get)

main = getCharDemoWithPrompt

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
      wordReversingIO

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

capsLockerIO = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStrLn $ map toUpper l

capsLockerIOV2 = do
  putStrLn "Enter some text and press Enter to see it in all caps:"
  putStrLn "Press Ctrl+D to exit."
  contents <- getContents
  putStr (map toUpper contents)

getCharDemoWithPrompt = do
  putStrLn "Enter a character(s):"
  getCharDemo

getCharDemo = do
  c <- getChar
  if c /= ' '
    then do
      putChar c
      getCharDemo
    else return ()

get3Colors = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          getLine
      )
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors

get3ColorsV2 = do
  colors <-
    mapM
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          getLine
      )
      [1, 2, 3, 4]
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors
