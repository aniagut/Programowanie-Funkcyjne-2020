import Data.Char
--funkcje jako argumenty
sumWith :: Num a => (a->a) -> [a] -> a
sumWith _ [] =0
sumWith f (x:xs) = f x +sumWith f xs

sum1= sumWith (\e ->e)
sumSqr= sumWith (\e->e^2)

--funkcje anonimowe - wyrazenia lambda
-- (\x y -> x+y)

--funkcje jako wyniki

--domkniecie funkcji
df :: (Double->Double)->Double->(Double->Double)
df f h = \x -> (f (x+h) - f(x-h))/(2*h)
 --sth= df (\x->x^2) 1
 --sth 1 =2

 --operatory zlozenia
 --(.)
 --(b -> c) -> (a -> b) -> a -> c
 --f . g = \x -> f (g x)
-- (f.g) 3

--operator $ - prawostronna lacznosc
-- ($) (a -> b) -> a -> b
--ghci> f = \x y -> x^2 + y^2
--ghci> f 1 2 + 3 -- 8
--ghci> f 1 $ 2 + 3 -- 26

--wzorzec collection pipeline 
--FILTER
--wyciaga tylko te spelniajace warunek
--(a -> Bool) -> [a] -> [a]
filter_ :: (a->Bool) -> [a] ->[a]
filter_ _ [] = []
filter_ p (x:xs)
    | p x = x:filter p xs
    | otherwise = filter p xs
--ghci >filter_ (\x->x<5) [1..10]
--[1,2,3,4]
-- filter p xs = [x | x<-xs, p x]

--MAP 
--aplikuje funkcje do kazdego elementu listy
--(a -> b) -> [a] -> [b]
--map _ [] = []
--map f (x:xs) = f x : map f xs
--ghci >map (\e -> e^2) [1..5]
--[1,4,9,16,25]
--map f xs = [f x | x<- xs]

--FOLDR
-- wykonuje rekurencyjnie operacje na tablicy
-- Foldable t => (a -> b -> b) -> b -> t a -> b
--f [] = z
--f (x:xs) = x `operator` (f xs)

foldr1 :: (a -> b -> b) -> b -> [a] -> b
foldr1 f z [] = z
foldr1 f z (x:xs) = f x (foldr f z xs)

--ghci >foldr (+) 0 [1..5]
--15

--FOLDL
--Foldable t => (b -> a -> b) -> b -> t a -> b
--idzie od gory
--f :: Num t => t -> [t] -> t
--f z [] = z
--f z (x:xs) = f (x `op` z) xs

foldl123 :: (b -> a -> b) -> b -> [a] -> b
foldl123 f z []  = z
foldl123 f z (x:xs) = foldl123 f (f z x) xs

--zipWith
--(a -> b -> c) -> [a] -> [b] -> [c]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

--zip -zlacza ze soba listy
--[a] -> [b] -> [(a, b)]
--unzip rozdziela na listy
--[(a, b)] -> ([a], [b])

--concat
--Foldable t => t [a] -> [a]
concat' :: [[a]] -> [a]
concat' = foldr (++) []
--ghci >concat' [[1,2],[3,4]]
--[1,2,3,4]

fun = sum.map((^2).length).filter (any isUpper).filter ((=='k').head).words

--[x^2+1 | x<- [1..10], odd x]
fun1 = map((+1).(^2)).filter odd 

fun2 :: String -> Int
fun2 = sum.
       map((^2).length).
       filter (all isUpper).
       filter ((=='K').head).
       words

a=sum [(x+1)^3 | x<- [1..10], 2*x<13,even x]
b= foldr (+) 0. map((^3).(+1)).filter even.filter((<13).(*2)) $ [1..10]
       
f g x =[g x, g(x+1), g(x+2)]
fprim = \g-> map g . (\x-> map ($ x) [id,(+1),(+2)])


o=sum [x^2 | x<- [1,3..15], x `mod` 3 ==1]
o' = foldr (+) 0.map(^2).filter((==1).(`mod` 3)).map(+1).map(*2) $ [0..7]

