module MyFunctor where

-- f is not concrete type but type constructor
-- (a -> b) is a function
-- (f a), (f b) are concrete types
class MyFunctor f where
  fMap :: (a -> b) -> f a -> f b

instance MyFunctor Maybe where
  fMap f (Just x) = Just (f x)
  fMap f Nothing = Nothing

-- Here [] is a type constructor for lists
instance MyFunctor [] where
  fMap = map
