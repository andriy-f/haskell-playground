module RPN (rpn) where
-- module for sovlving RPN expressions

import Data.List (foldl)

-- | Function for solving RPN expressions
rpn :: String -> Float
rpn =
  head . foldl foldFun [] . words
  where
    foldFun (x : y : ys) "*" = (x * y) : ys
    foldFun (x : y : ys) "+" = (x + y) : ys
    foldFun (x : y : ys) "-" = (y - x) : ys
    foldFun (x : y : ys) "/" = (y / x) : ys
    foldFun (x : y : ys) "^" = (y ** x) : ys
    foldFun (x : xs) "ln" = log x : xs
    foldFun xs "sum" = [sum xs]
    foldFun xs numberString = (read numberString) : xs
