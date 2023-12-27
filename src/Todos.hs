module Todos (main) where

import Data.List (lookup, delete)
import System.Environment (getArgs)
import System.Directory (renameFile)

main = do
  putStrLn "Welcome to todos app"
  (command : args) <- getArgs
  let maybeAction = lookup command dispatch
      action = getActionOrDefault maybeAction
  action args

dispatch :: [(String, [String] -> IO ())]
dispatch =
  [ ("view", viewAction),
    ("add", addAction),
    ("remove", removeAction),
    ("help", unknowsAction)
  ]

getActionOrDefault :: Maybe ([String] -> IO ()) -> ([String] -> IO ())
getActionOrDefault (Just action) = action
getActionOrDefault Nothing = unknowsAction

unknowsAction :: [String] -> IO ()
unknowsAction _ = do
  putStrLn "Command not found"
  helpAction

helpAction = do
  putStrLn "Usage: todos <command> <filename>"
  putStrLn "i.e: todos view todos.txt"

viewAction :: [String] -> IO ()
viewAction args = do
  putStrLn "Viewing todos"
  tasksAsString <- readFile (head args)
  let tasks = lines tasksAsString
      tasksWithNumbers = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] tasks
  putStrLn (unlines tasksWithNumbers)

addAction (fileName : task : _) = do
  putStrLn ("Adding todo '" ++ task ++ "' to '" ++ fileName ++ "'...")
  appendFile fileName (task ++ "\n")

removeAction [fileName, taskNumStr] = do
  putStrLn ("Removing todo #" ++ taskNumStr ++ " from '" ++ fileName ++ "'...")
  tasksAsString <- readFile fileName
  let tasks = lines tasksAsString
      taskNum = read taskNumStr
      newTasks = delete (tasks !! taskNum) tasks
      newTasksAsString = unlines newTasks
  writeFile (fileName ++ ".new") newTasksAsString
  renameFile (fileName ++ ".new") fileName
