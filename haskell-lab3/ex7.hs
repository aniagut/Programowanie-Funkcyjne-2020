onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
 | x `mod` 2 == 1 = x : onlyOdd xs
 | otherwise      = onlyOdd xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
 | p x = x : filter' p xs
 | otherwise  = filter' p xs

onlyEven' xs = filter' even xs

onlyOdd' xs = filter' odd xs

--length $ onlyEven [1..10^6]
--length $ filter even [1..10^6]

--length ([x | x<-[1..10^6], even x])