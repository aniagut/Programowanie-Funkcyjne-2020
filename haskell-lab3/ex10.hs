isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = foldr (&&) True (map (\(a,b) -> a<=b) (zip xs (tail xs)))

everySecond :: [t] -> [t]
everySecond xs = [ x | (x, n) <- (zip xs [1..]), odd n ]
