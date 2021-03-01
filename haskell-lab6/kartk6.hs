--triples n = [(a,b,c) | a<- [1..n], b<- [1..n], c<- [1..n], (a^2+b^2) `mod`  c^2 ==1]
import Control.Monad
triples' n = do
    let vals = [1..n]
    a <- vals
    b <- vals
    c <- vals
    guard $ (a^2 + b^2) `mod` c^2 ==1
    return [(a,b,c)]

--(>>) -  Monad m => m a -> m b -> m b
-- return -  Monad m => a -> m a
--(>=>) - Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- return "Ala" - Monad m => m [Char]
-- (>>=) -  Monad m => m a -> (a -> m b) -> m b
-- return 1 - (Monad m, Num a) => m a
-- return 'a' - Monad m => m Char
-- return "a" -  Monad m => m [Char]
-- return True - Monad m => m Bool

--triples1 n = [(a,b,c) | a<-[1..n], b<-[1..n], c<-[1..n], a^2-a*b+b^2==c^2]

triples11 n = do
    let vals = [1..n]
    a <- vals
    b<- vals
    c<- vals
    guard $ (a^2-a*b+b^2==c^2)
    return [(a,b,c)]

--join $ (((2:) . return ) >=> \x -> [x-1, x+1]) <$> [1..3]
-- zwroci [1,3,0,2,1,3,1,3,1,3,2,4]

tryFactorial :: Int -> Maybe Int
tryFactorial 0 = Just 1
tryFactorial n =
    if n<0 then Nothing
    else do
        prev <- tryFactorial $ n-1
        return $ n*prev

triples2 n = do
    let vals = [1..n]
    a<- vals
    b<- vals
    c<- vals
    guard $ a^2+b^2==c^2
    return [(a,b,c)]