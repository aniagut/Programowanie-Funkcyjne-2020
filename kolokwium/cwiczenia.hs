import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
--kartkowka 2
--zad1
f=(*2)
--zad 2
--[(x^) y | x<- [1..3], y<- (:[2]) (x+3), odd(x-y)]
--[1,1,32,729,9]
sumDivisibleBy3 :: Integral a => [a] ->a
sumDivisibleBy3 = loop 0
    where loop acc [] = acc 
          loop acc (x:xs) =
              if x `mod` 3 == 0 then loop (x^2 + acc) xs 
              else loop (acc) xs

--kartkowka 3
--zad1
g= \x -> x*5
--zad2
fun = sum .
    map ((^2).length).
    filter (any isUpper) .
    filter ((=='k') . head).
    words

--zad3
f1 = [x^2+1 | x<- [1..10], odd x]
f1' = map ((+1).(^2)).
      filter odd $ [1..10]

--kartkowka 4
--zad 2
data Box a = MkBox {value::a}
instance Eq a => Eq (Box a) where 
    MkBox {value=v}==MkBox {value=v'} = v==v'

--kartkowka 5
--zad1

--foldr (+) 0 ((*) <$> ZipList [1,2,3] <*> (fmap (+1) $ ZipList [4,5,6]))
--38

--zad2
f2=do
    putStrLn "Hello"
    s<- getLine
    putStrLn $ (++ "!") s

f2' = putStrLn "Hello" >>
            getLine >>=
            \n -> putStrLn (n ++ "!")

--kartk 6
--zad1
tryFactorial n = do
    guard ( n>=0 )
    if n == 0 then return (Just 1)
    else (n*) $ (tryFactorial $ n-1)

--join $ (((2:) . return ) >=> \x -> [x-1, x+1]) <$> [1..3]

f3 n =[(a, b, c) | a<-[1..n], b<-[1..n],c<-[1..n], a^2-a*b+b^2==c^2]

f3' n = do
    let vals = [1..n]
    a<- vals
    b<- vals
    c<- vals
    guard $ a^2+b^2-a*b==c^2
    return [(a,b,c)]