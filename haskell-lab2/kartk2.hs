--curry-rozplątuje

add2C :: Num a=> (a,a)->a
add2C (x,y)=x+y

--curry add2C x y

--uncurry zaplątuje

add2T :: Num a=>a->a->a
add2T x y=x+y

--uncurry add2T (x,y)

fivetoPower_ :: Integer->Integer
fivetoPower_ = (5^)

--flip (>) 4 5 -True

--LISTY
--import Data.List
--let xs = 1:2:3:4:[]
--let xs = [1..4]
--length xs
--reverse xs
--head xs-1 element
--tail xs-wszystkie oprocz 1
--last xs
--init xs-wszystkie oprocz ostatniego
--0:xs -dodawanie z przodu
--xs ++ [6]-dodawanie z tylu
--xs !! n - element na indeksie n
--take n xs-pierwsze n
--drop n xs - bez pierwszych n
-- any (>0) xs
-- all (>0) xs
--zip xs ['a','b']
--spllitAt n- rozdziela liste na 2 przed indeksem n
--sort xs
--n `elem` xs-sprawdza czy jest element o wartosci n
--minimum xs
--maximum xs
--product xs-mnozenie

getElematIdx :: [Char]->Int->Char
getElematIdx xs n = xs !! n

getElematIdx1 :: [Int]->Int->Int
getElematIdx1 xs n= head(drop n xs)

--[x^3 | x<-[1..5]]
--[x^2 | x <- [1..5], x ^ 2 - x > 3]      9 16 25
--[(i,j) | i <- [0..3], j <- [0..2]]     - wszystkie pary
-- [(i,j) | i <- [0..3], j <- [0..i]]    -j<=i
--[(i+j)^2 | i <- [0..3], j <- [0..2]]
--[i+j | i <- [0..4], j <- [i..4], (i + j) `mod` 3 == 0] 
--[(a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10], a ^ 2 + b ^ 2 == c ^ 2]

 --rekurencja
 
fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

--ZAD z kartkówki
--funkcja do sumowania kwadratów elementów listy
sumsquare :: Num a => [a]->a
sumsquare xs = loop 0 xs
 where loop acc [] = acc
       loop acc (x:xs) = loop(x^2+acc) xs

--[a*b | a<-[1..8], b<-[a..8], odd(a-b)]

--wynik ewaluacji

--zip [1..] (reverse (take 3 (2: [2..])))

--[(1,3),(2,2),(3,2)]

-- let f = (+) 3

--reverse(take 5 (0: []++[2..])) !! 3   - 2

--[a-b | a<-[1..5],b<-[1..a-1], even(a+b)]

