--przetwarzanie list
--funkcje nie moga zaczynac sie wielka litera

--curry - ((a, b) -> c) -> a -> b -> c
-- uncurry (a -> b -> c) -> (a, b) -> c

addT :: Num a=> (a,a)->a
addT (x,y) = x+y

--(curry addT) 3 4

--partialy applied functions
addC :: Num a=> a->a->a->a
addC x y z =x+y+z

add1To_ :: Num a=> a->a->a
add1To_ = addC 1

add1and2To_ :: Num a=>a->a
add1and2To_ = add1To_ 2

add1and2and3 :: Num a=> a
add1and2and3 = add1and2To_ 3

-- flip - (a -> b -> c) -> b -> a -> c

--twoToPower_ = (2^)

substr5From = flip (-) 5

--leniwe obliczanie/wartosciowanie
f x = 42
-- f (9/0) = 42

--let x=1+2
-- :sp x = _

-- import Data.Tuple do list

--zipWith (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith (+) [1,2,3] [3,2,1] = [4,4,4]

--rekursja i przetwarzanie list

--length xs
--reverse xs
--head xs
--tail xs - wszystko oprocz pierwszego
--last xs - ostatni
--init xs -- wszystko oprocz ostatniego
-- 0:xs
--xs ++ [5]
--xs !! 2 - element na 2 pozycji (czyli 3 bo od 0)
-- take 2 xs - pierwsze 2
--drop 2 xs - bez pierwszych dwoch
--null xs -czy pusta
--any (>2) xs
--all (>2) xs
--zip xs ['a','b','c'] = [(1,'a'),(2,'b'),(3,'c')]
-- splitAt 2 xs = ([1,2],[3,4,5,6,7,8,9])
-- sort [4,3,1]
--2 `elem` xs -czy jest taki element w liscie
--filter even - tylko parzyste
--filter odd
--map (*2) xs wszystkie razy 2
--foldr (+) 0 xs- dodanie wszystkich
--minimum xs, maxiumum xs,sum xs


--LIST COMPREHENSIONS
-- xs=[x^2 | x<- [1..10], x^5<1025]
--[(x,y) | x<-[1..3], y<-[1..3]]

--REKURSJA 

fibb :: (Num a, Eq a) => a->a
fibb n =
    if n==0 || n==1 then n
    else fibb (n-2) + fibb (n-1)

sum1 :: Num a=> [a]->a
sum1 [] = 0
sum1 (x:xs) = x + sum xs

qSort :: Ord a=> [a]->[a]
qSort [] = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort(rightPart xs)
    where
        leftPart xs=[y | y<- xs, y<=x]
        rightPart xs=[y| y<- xs,y>x]

prod' :: Num a=>[a]->a
prod' = loop 1 where
    loop acc [] =acc
    loop acc (x:xs) = loop (x*acc) xs

sumDivisibleBy3 :: Integral a => [a] -> a
sumDivisibleBy3 = loop 0 where
    loop acc [] = acc
    loop acc (x:xs) = if x `mod` 3==0
                      then loop (x^2+acc) xs
                      else
                          loop (acc) xs

