module MyMonad where

import Control.Monad

class MyMonad m where
  myReturn :: x -> m x
  myBind :: m a -> (a -> m b) -> m b

instance MyMonad ((->) r) where
  myReturn x = \_ -> x
  h `myBind` f = \w -> f (h w) w
