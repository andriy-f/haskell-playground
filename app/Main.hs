module Main where

import Control.Monad (forM, forever, mapM, when)
import Data.Char (toUpper)
import Data.List (delete, elemIndex, find)
import MyByteStrings (myCopyFile)
import PlayTypes (Vector3D (..), vPlus)
import qualified Randomization
import System.Directory (doesFileExist, removeFile, renameFile)
import System.Environment (getArgs, getProgName)
import System.IO
  ( IOMode (ReadMode, WriteMode),
    hClose,
    hGetContents,
    openFile,
    withFile,
  )
import qualified Todos

main = main2

main2 = do
  args <- getArgs
  let parseRes = parseArgs args
  if null parseRes
    then showHelp
    else dispatch (head parseRes)

showHelp = do
  putStrLn "Usage:"
  putStrLn "  -h - show help"
  putStrLn "  -i - interactive mode"
  putStrLn "  -sr <subprogram> - run subprogram <subprogram>"

dispatch :: (String, ArgValue) -> IO ()
dispatch ("-h", _) = showHelp
dispatch ("-i", _) = interactiveMode

dispatcch ("-sr", ToFill) = do
  putStrLn "Subprogram not filled, Usage:  -sr <subprogram> - run subprogram <subprogram>"
dispatcch ("-sr", Value a) = do
  putStrLn "Running subprogram, this is TODO" -- ++ show a

data ArgValue = NoValue | Value String | ToFill deriving (Show, Eq)

parseArgs :: [String] -> [(String, ArgValue)]
parseArgs args =
  let argWithNoValue x = x `elem` ["-h", "-i"]
      argWithValue x = (x == "-sr")
   in foldl
        ( \acc arg ->
            if not (null acc) && snd (head acc) == ToFill
              then (fst $ head acc, Value arg) : tail acc -- fill value of previously found arg
              else
                if argWithNoValue arg
                  then (arg, NoValue) : acc -- arg with no expected value
                  else
                    if argWithValue arg
                      then (arg, ToFill) : acc -- arg with flag that it must be filled later
                      else acc -- actuelly this is some errorneous situation
        )
        []
        args

interactiveMode = do
  putStrLn "Interactive mode"
  putStrLn "Commands:"
  putStrLn "1. exit - exit interactive mode"
  putStrLn "2. capslocker"
  putStrLn "3. randoms - run randoms app"
  putStrLn "4. copyfile - copy file from source to destination"
  putStrLn "Enter command number (1,2,...):"
  command <- getLine
  case command of
    "1" -> return ()
    "2" -> capsLockerIOV2
    "3" -> Randomization.main
    "4" -> copyFileInteractive
    _ -> do
      putStrLn $ "Unknown command: " ++ command ++ "\n"
      interactiveMode

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

copyFileInteractive = do
  putStrLn "Enter source file name:"
  source <- getLine
  putStrLn "Enter destination file name:"
  dest <- getLine
  myCopyFile source dest

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

capsLockerSilent = do
  contents <- getContents
  putStr (map toUpper contents)

-- This works lazily,
-- so if usind stdin, it will print result after each input line
shortLinesIO = do
  input <- getContents
  putStr (shortLinesOnly input)

shortLinesIOV2 = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
  let linesArray = lines input
      shortLines = filter (\ln -> length ln < 10) linesArray
      shortLinesConcatenated = unlines shortLines
   in shortLinesConcatenated

forMapDemo = do
  colors <-
    forM
      [1, 2, 3, 4]
      ( \a -> do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          color <- getLine
          return color
      )
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors

getCharDemoWithPrompt = do
  putStrLn "Enter character(s), mixed with spaces:"
  getCharDemoV2

getCharDemo = do
  c <- getChar
  if c /= ' '
    then do
      putChar c
      getCharDemo
    else return ()

getCharDemoV2 = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    getCharDemoV2

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

respondPalindromeStatusIO = interact respondPalindromeStatus

respondPalindromeStatus = unlines . map isPalindromeTextual . lines

isPalindromeTextual line = if isPalindrome line then "Pal" else "Not pal"

isPalindrome xs = xs == reverse xs

readAndPrintFile = do
  handle <- openFile "mantra.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

readAndPrintFileV2 = do
  withFile
    "mantra.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

todoRoutineIO = do
  existingTodos <- readFile "todos.txt"
  putStrLn $ "These are your TO-DO items:\n" ++ existingTodos
  putStrLn "What do you want to do next?"
  newTodos <- getContents
  appendFile "todos.txt" newTodos
  return ()

todoEditorIO = do
  let todoFileName = "todos.txt"
  fileExist <- doesFileExist todoFileName
  if fileExist
    then do
      oldContent <- readFile todoFileName
      putStrLn $ "These are your TO-DO items:\n" ++ addLineNumbers oldContent
    else writeFile todoFileName ""
  forever $ do
    putStrLn "What do you want to do next? (add / remove / print / exit)"
    command <- getLine
    case command of
      "add" -> do
        putStrLn "Enter a new TO-DO item:"
        todoItem <- getLine
        appendFile todoFileName (todoItem ++ "\n")
      "remove" -> do
        putStrLn "Enter the number of item to remove:"
        numberString <- getLine
        oldContent <- readFile todoFileName
        let number = read numberString
            oldTodos = lines oldContent
            newTodos = delete (oldTodos !! number) oldTodos
            newContent = unlines newTodos
        writeFile (todoFileName ++ ".new") newContent
        renameFile (todoFileName ++ ".new") todoFileName
      "print" -> do
        content <- readFile todoFileName
        putStrLn $ "These are your TO-DO items:\n" ++ addLineNumbers content
      "exit" -> return ()
      _ -> putStrLn "Unknown command"

addLineNumbers contents =
  let list = lines contents
      newList = zipWith (\n line -> show n ++ ". " ++ line) [0 ..] list
   in unlines newList

getProgramInfo = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM putStrLn args
  putStrLn "The program name is:"
  putStrLn progName
