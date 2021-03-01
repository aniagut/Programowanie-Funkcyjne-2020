import Data.Char
import Control.Monad
collatz :: Int -> Int 
collatz n =
    let divides d n = n `mod` d ==0
        isEven n = divides 2 n
    in if isEven n then n `div` 2
                   else 3*n+1

f (x,y,z) = if x+3<z
              then True
              else y/=y

--[a-b| a<-[1..5],b<-[1..a-1],even(a+b)]  

sumSquares :: Num a=> [a]->a
sumSquares = loop 0
    where loop acc [] = acc
          loop acc (x:xs) = loop (x^2 +acc) xs

--[(a*b) | a<-[1..8],b<-[1..a],odd(a-b)]
a=sum [(x+1)^3 | x<- [1..10], 2*x < 13, even x]
a'=foldr (+) 0.map((^3).(+1)).filter even.filter((<13).(*2)) $ [1..10]

fun :: String -> Int 
fun = sum.
      map ((^2).length).
      filter (all isUpper).
      filter ((=='K').head).
      words

f1 g x = [g x, g (x+1), g (x+2)]
f' = \g -> map g.(\x-> map ($ x) [id, (+1), (+2)])

fun1 :: String -> Int
fun1 = length.
       filter ((>2).length).
       map(filter(`elem` vowels)).
       words
  where vowels = "aeyiou"

b=sum [x^2 | x<- [1,3..15], x `mod` 3 ==1]
b'=foldr (+) 0.map(^2).filter((==1).(`mod` 3)).map(+1).map(*2) $ [0..7]

data Tree a = Node (Tree a) a (Tree a)
            | Leaf a
toList :: Tree a -> [a] 
toList (Leaf x) = [x]
toList (Node left x right) = toList left ++ [x] ++ toList right

data Tree' a = Node' (Tree' a) a (Tree' a)
             | Leaf'
sumSq :: Num a => Tree' a ->a
sumSq Leaf' = 0
sumSq (Node' left x right) = x^2 + sumSq left + sumSq right

data Data = Data {first::String, second:: String}

data Foo a = MkFoo {value::a,name::String}
instance Show a => Show (Foo a) where
    show (MkFoo {value=v,name=n}) = "Foo " ++ show n ++ " with " ++ show v

newtype Box a = MkBox {valueInside :: a}
instance Show a => Show (Box a) where
    show (MkBox {valueInside=v}) = "Box with " ++ show v

mamo = do
    s<-getLine
    n<- return 3
    putStrLn $ show n ++ s

aba=getLine >>= \s -> return 3 >>= \n -> putStrLn $ show n ++ s

data Tree1 a = Node1 a (Tree1 a) (Tree1 a)
            | Leaf1 
paths :: Tree1 a -> [[a]]
paths Leaf1 = pure []
paths (Node1 a lt rt) = concat $ ([(a:)] <*>) <$> (fmap paths [lt,rt])

pathsSum :: Num a=> Tree1 a -> [a]
pathsSum Leaf1 = pure 0
pathsSum (Node1 a lt rt) = concat $ ([(a +)] <*>) <$> (fmap pathsSum [lt,rt])

sumLog :: [Double] -> Maybe Double 
sumLog [] = Just 0
sumLog (x:xs) =
    if x<= 0 then Nothing
    else do 
        s <- sumLog xs 
        return $ s + log x

triples n = [(a,b,c) | a<- [1..n], b<- [1..n], c<-[1..n], (a^2+b^2) `mod` c^2 ==1]
triples' n = do 
    let vals = [1..n]
    a<- vals
    b<- vals 
    c<- vals
    guard $ (a^2+b^2) `mod` c^2 ==1
    return [(a,b,c)]

tryFactorial :: Int -> Maybe Int
tryFactorial 0 = Just 1 
tryFactorial n =
    if n< 0 then Nothing 
    else do
        prev <- tryFactorial $ n-1
        return $ n*prev
newtype  Foo a = MkFoo {val :: a }

instance Show a  => Show (Foo a)  where
  show (MkFoo {val=v}) = "Foo with " ++ show v

f g x = [ g x, g (x ^2), g (x^3) ]