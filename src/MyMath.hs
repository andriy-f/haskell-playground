module MyMath (myGCD, myGCDWithLog, myGCDWithMonadicLog) where

import Control.Monad.Writer (Writer, tell)
import MyDiffList (MyDiffList, fromMyDiffList, toMyDiffList)

myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b = myGCD b (a `mod` b)

myGCDWithLog :: Int -> Int -> (Int, [String])
myGCDWithLog a 0 = (a, ["GCD " ++ show a ++ " " ++ show 0 ++ " = " ++ show a])
myGCDWithLog a b = (gcd, log2)
  where
    (gcd, log) = myGCDWithLog b (a `mod` b)
    newLog = "GCD " ++ show a ++ " " ++ show b ++ " = " ++ show gcd
    log2 = newLog : log

myGCDWithMonadicLog :: Int -> Int -> Writer [String] Int
myGCDWithMonadicLog a 0 = do
  tell ["Finished with " ++ show a]
  return a
myGCDWithMonadicLog a b = do
  tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
  myGCDWithMonadicLog b (a `mod` b)

myGCDWithMonadicLog' :: Int -> Int -> Writer (MyDiffList String) Int
myGCDWithMonadicLog' a 0 = do
  tell (toMyDiffList ["Finished with " ++ show a])
  return a
myGCDWithMonadicLog' a b = do
  tell (toMyDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
  myGCDWithMonadicLog' b (a `mod` b)
