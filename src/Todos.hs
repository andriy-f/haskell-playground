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
dispatch = [("view", view), ("help", unknowsAction)]

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

view :: [String] -> IO ()
view args = do
  putStrLn "Viewing todos"
  putStrLn $ "Args:" ++ unlines args
