module MyFunctor where

import BinaryTreeModule (BinaryTree (..))
import qualified Data.Map as Map

-- f is not concrete type but type constructor
-- (a -> b) is a function
-- (f a), (f b) are concrete types
class MyFunctor f where
  myFMap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
  myFMap f (Just x) = Just (f x)
  myFMap f Nothing = Nothing

-- Here [] is a type constructor for lists
instance MyFunctor [] where
  myFMap = map

instance MyFunctor BinaryTree where
  myFMap f EmptyTree = EmptyTree
  myFMap f (Node x left right) = Node (f x) (myFMap f left) (myFMap f right)

instance MyFunctor (Either a) where
  myFMap f (Right x) = Right (f x)
  myFMap f (Left x) = Left x

instance MyFunctor (Map.Map k) where
  myFMap = Map.map

instance MyFunctor IO where
  myFMap f action = do
    result <- action
    return (f result)

-- (->) r is a type constructor for functions (r is first argument)
instance MyFunctor ((->) r) where
  myFMap f a = f . a -- or just myFMap = (.)

functorExample :: IO ()
functorExample = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"

functorExample2 =
  let funArr = fmap (*) [1, 2, 3, 4]
      result = fmap (\f -> f 9) funArr
   in result

class (Functor f) => MyApplicative f where
  myPure :: a -> f a
  mySuperMap :: f (a -> b) -> f a -> f b

-- mySuperMap is like <*>
instance MyApplicative Maybe where
  myPure = Just
  mySuperMap (Nothing) _ = Nothing
  mySuperMap (Just a) something = fmap a something

instance MyApplicative ((->) r) where
  myPure x = (\_ -> x)
  mySuperMap f g = \x -> f x (g x)

applicativeFunctorExample = mySuperMap (Just (* 2)) (Just 2)
