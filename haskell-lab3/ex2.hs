sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []    = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a->a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

--sum = sumWith (\x->x)
--sumSqr = sumWith (\x->x^2)
--sumCube = sumWith (\x->x^3)
--sumAbs = sumWith (\x-> if x>=0 then x else (-x))

--sumWith (^5) [1..15]

--listLength=sumWith (\x->1)

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs


