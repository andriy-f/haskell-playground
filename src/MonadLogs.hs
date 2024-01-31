module MonadLogs
  ( applyLog,
    applyLog2,
    applyLog3,
    addDrink,
    addDrinkMultiple,
  )
where

import Data.Monoid (Sum)

-- basic appllyLog
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- more generic applyLog
applyLog2 :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog2 (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- Monoidal applyLog
applyLog3 :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog3 (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

type Food = String

type Price = Sum Int

-- adds milk to beans
-- adds rice milk to oats
-- adds borjomi to everything else
addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", 25)
addDrink "oats" = ("rice milk", 99)
addDrink _ = ("borjomi", 30)

-- addDrink ad
addDrink2beans :: (Food, Price)
addDrink2beans = ("beans", 10) `applyLog3` addDrink

addDrinkMultiple :: (Food, Price)
addDrinkMultiple = ("beans", 10) `applyLog3` addDrink `applyLog3` addDrink `applyLog3` addDrink
