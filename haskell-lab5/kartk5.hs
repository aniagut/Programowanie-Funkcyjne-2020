-- <$>
--Prelude> (*2) <$> [1..3]
--[2,4,6]
--Prelude> show <$> Just 11
--Just "11"

--fmap 
--fmap (*2) [1..5]
--[2,4,6,8,10]

-- <$
-- (<$) :: a -> f b -> f a
-- 'a' <$ [1..5]
-- "aaaaaa"

--pure
--pure 1 :: [Double]
--[1.0]

-- <*>
--  [(+1), (*2)] <*> [1,2,3]
-- [2,3,4,2,4,6]

--(*) <$> [1,2,3] <*> [100,101,102]
--[100,101,102,200,202,204,300,303,306]


--(<*) :: f a -> f b -> f a 
--[1..2] <* [11..15]
--[1,1,1,1,1,2,2,2,2,2]

-- (*>) :: f a -> f b -> f b
--[1..2] *> [11..15]
--[11,12,13,14,15,11,12,13,14,15]

--ghci> (filter odd) <$> Left [1..3]
--Left [1,2,3]

--ghci> (\x y -> max <$> x <*> y) <$> [Just 1, Just 2] <*> [Just 3, Nothing]
--[Just 3,Nothing,Just 3,Nothing]

--ghci> (\x y z -> (x == y, z == y)) <$> Right 1 <*> Right 2 <*> Left 2
--Left 2

--ghci> pure (<) <*> Right 1 <*> pure 3
--Right True



data Tree a = Node a (Tree a) (Tree a)
            | Leaf

pathsSum:: Num a => Tree a -> [a]
pathsSum Leaf = pure 0
pathsSum (Node a lt rt) = concat $ ([(a +)] <*>) <$> (fmap pathsSum [lt,rt])

paths:: Tree a -> [[a]]
paths Leaf = pure []
paths (Node a lt rt) = concat $ ([(a :)] <*>) <$> (fmap paths [lt,rt])

sth=getLine >>= \s -> return 3 >>= \n -> putStrLn $ s ++ show n

doSTh = do
    s <- getLine
    n <- return 3
    putStrLn $ s ++ show n

   -- fmap :: Functor f => (a -> b) -> f a -> f b
   --(<*) :: Applicative f => f a -> f b -> f a
   --(*>) :: Applicative f => f a -> f b -> f b
   --(<$>) :: Functor f => (a -> b) -> f a -> f b
   --pure :: Applicative f => a -> f a
   --(<$) :: Functor f => a -> f b -> f a
   --(<*>) :: Applicative f => f (a -> b) -> f a -> f b