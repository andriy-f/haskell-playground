-- define module
module MyTypeClasses where

import BinaryTreeModule (BinaryTree (..))

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

-- YesNo
class YesNo a where
  yesNo :: a -> Bool

instance YesNo Int where
  yesNo 0 = False
  yesNo _ = True

instance YesNo [a] where
  yesNo [] = False
  yesNo _ = True

instance YesNo Bool where
  yesNo = id

instance YesNo (Maybe a) where
  yesNo (Just _) = True
  yesNo Nothing = False

instance YesNo (BinaryTree a) where
  yesNo EmptyTree = False
  yesNo _ = True

instance YesNo TrafficLight where
  yesNo Red = False
  yesNo Yellow = False
  yesNo _ = True

yesNoIf :: (YesNo y) => y -> a -> a -> a
yesNoIf yesNoVal yesResult noResult = if yesNo yesNoVal then yesResult else noResult
