--IO input output

--ghci> let retA = return 'a'
--ghci> retA
--'a'

--ghci> let echo = getChar >>= putChar
--ghci>  echo
--mm

-- notacja do
main :: IO ()
main = putStrLn "Hello,World!"

--(>>)
--Monad m => m a -> m b -> m b

main1=putStr "Hello" >>
        putStrLn " World"

main2=do
    putStr "Hello"
    putStrLn " World"

--(>>=)
--Monad m => m a -> (a -> m b) -> m b
main3 = putStrLn "Your name?">>
         getLine >>=
         \n -> putStrLn ("Hello, " ++n)

main4=do
    putStrLn "Your name?"
    n<- getLine
    putStrLn ("Hello, " ++ n)

main5 = do 
    a<- return "a"
    b<- return "b"
    return ()
    return 1
    putStrLn $ a ++ " " ++ b

main6=do
    let a = "a"
        b = "b"
    return ()
    return 1
    putStrLn $ a ++ " " ++ b

--reku

getLine' :: IO String 
getLine'= do 
    x<- getChar
    if x =='\n'
    then return []
    else do 
        xs<- getLine
        return (x:xs)

main7=do
    line<-getLine' 
    putStrLn line

putStr' :: String -> IO()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs
putStrLn' :: String->IO()
putStrLn' xs = do putStr' xs 
                  putChar '\n' 

--fmap :: Functor f => (a -> b) -> f a -> f b
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- pure :: Applicative f => a -> f a
-- (<$) :: Functor f => a -> f b -> f a
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- (<*) :: Applicative f => f a -> f b -> f a
-- (*>) :: Applicative f => f a -> f b -> f b

