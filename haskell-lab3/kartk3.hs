--Napisać przy użyciu funkcji lambda: (*5)
--let f= \x ->(x*5)
import Data.Char


fun = sum . map ((^2). length) . filter (any isUpper) .filter ((== 'k') . head) . words
--najpierw zrobi tablice wyrazów, potem wybierze te które zaczynaja sie na k, 
--potem z nich te, które maja jakies duze litery, potem doda do siebie te dlugosci
--podniesione do kwadratu

--Przepisać przy użyciu map, filter, fold:
--product [x^2+1 | x <- [1..10], odd x]
f1=product . map((+1).(^2)) . filter ( (<10) ) . filter (odd)

--Podstawowe klasy typów
--Eq – dla typów używanych w porównaniach ( ==,/= )
--Ord – dla typów używanych w porządkowaniu wartości (
-- <, >, <=, >= )
--Show – dla typów które mogę prezentowane jako String
--Num – dla wszystkich typów numerycznych
--Integral – dla typów liczb całkowitych
--Floating – dla typów liczb zmiennoprzecinkowych
--Enum – dla typów wyliczalnych

--FOLDL
--it takes the second argument and the first item of the list and applies the function to them, then feeds the function with this result and the second argument and so on. See scanl for intermediate results.
--foldl (+) 1 . map(*2) . filter(>99) $ [100, 10, 500, 90]

--FOLDL1
--it takes the first 2 items of the list and applies the function to them, then feeds the function with this result and the third argument and so on.
--(foldl1 (++) . map(\x -> const (show x) "1")) [10^x | x <- [0..100], x<3]

--FOLDR
--it takes the second argument and the last item of the list and applies the function, then it takes the penultimate item from the end and the result, and so on.

--FOLDR1
--it takes the last two items of the list and applies the function, then it takes the third item from the end and the result, and so on.
--foldr1 (||) . map odd . map ($ 1) $ [id, const 2, (+2)]

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = let z' = z `f` x
                    in foldl' f z' xs

--let f = \x-> (++) x "!"

--sum [ x^3 | x<-[1..10], x `mod` 3 == 0 ]


--
f g x = [g x, g (x+1), g (x+2)]

f10= \g -> map g . (\x -> map ($ x) [id,(+1),(+2)])

--
a= sum [x^2 | x<- [1,3..15],x `mod` 3 ==1]

b= foldr (+) 0 (map(^2).filter((==1).(`mod` 3)). map (+1).map(*2) $ [0..7])

fun11 :: String -> Int
fun11 = length.filter((>2).length).map(filter(`elem` vowels)).words
    where vowels = "aeyiou"


map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs

foldl'' :: (b->a->b) -> b -> [a] -> b
foldl'' f z [] = z
foldl'' f z (x:xs) = foldl'' f (f z x) xs

--let f = \x-> x^2

--let f3= \x-> \(y,z)->(x,y,z)    ==   f x = \(y,z) -> (x,y,z)

zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--let f = \x -> 2:x

--foldr (+) 1 . map ((+1) . (*2)) . filter even $ [0..5]   --16



--foldr1 (+) . map (sum.(\x->[x,1]).($ 1)) $ zipWith (.) [(2*), (3*)] [(1+), (2+)]  --15


a11=foldr (+) 0 . map((^3).(+1)).filter even.filter((<13).(*2))


f17= \g-> map g . (\x-> map ($ x) [id, (+1),(+2)])

fun10 :: String -> Int
fun10 = sum . map((^2).length).filter (all isUpper) . filter((=='K').head). words