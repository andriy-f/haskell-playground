module Todos (main) where

import Data.List (lookup)
import System.Environment (getArgs)

main = do
  putStrLn "Welcome to todos app"
  (command : args) <- getArgs
  let
    maybeAction = lookup command dispatch
    action = getActionOrDefault maybeAction
  action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [("view", viewAction), ("help", unknowsAction)]

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
  readFile (head args) >>= putStrLn
