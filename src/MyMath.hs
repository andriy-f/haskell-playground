module MyMath (myGCD) where

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
