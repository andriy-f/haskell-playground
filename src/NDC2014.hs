module NDC2014
    ( printList
    )
where

printList :: Show a => [a] -> [Char]
-- printList [] = []
printList = foldr (\x acc -> show x ++ acc) ""
