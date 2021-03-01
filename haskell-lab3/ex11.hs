concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs