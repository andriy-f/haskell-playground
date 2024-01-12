module RPN (rpn) where
-- module for sovlving RPN expressions

import Data.List (foldl)

-- | Function for solving RPN expressions
rpn :: (Num a, Read a) => String -> a
rpn =
  head . foldl foldFun [] . words
  where
    foldFun (x : y : ys) "*" = (x * y) : ys
    foldFun (x : y : ys) "+" = (x + y) : ys
    foldFun (x : y : ys) "-" = (y - x) : ys
    foldFun xs numberString = (read numberString) : xs
