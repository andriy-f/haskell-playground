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

-- (j a) is concrete type, a is concrete type,
-- so this means that j has type * -> *
--
-- (t a j) is concrete type, a is concrete type, j has type * -> *,
-- so this means that t has kind * -> (* -> *) -> *
class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

-- :t Frank {frankField = Just "Ha"}
-- Frank {frankField = Just "Ha"} :: Frank [Char] Maybe
instance Tofu Frank where
  tofu x = Frank x

-- Let's make it type of Funtor
data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}

newtype CharList = CharList {getCharList :: [Char]} deriving (Eq, Show)

newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)
