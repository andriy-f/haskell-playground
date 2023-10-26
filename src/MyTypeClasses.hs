-- define module
module MyTypeClasses where

class MyEq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x MyTypeClasses./= y)
  x /= y = not (x MyTypeClasses.== y)

data TrafficLight = Red | Yellow | Green

instance MyEq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
