doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems []   = []
sqrElems (x:xs) = x^2 : sqrElems xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' xs = map' (*2) xs
sqrElems' xs    = map' (^2) xs

--list comprehensions
doubleElems'' xs = [x*2 | x<-xs]
sqrElems'' xs = [x^2 | x<-xs]

