sumWith g [] = 0
sumWith g (x:xs) = g x + sumWith g xs 

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x : xs) =  f x (foldr' f z xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0

prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = let z' = z `f` x
                    in foldl f z' xs

sumWith''' g  = foldl' (\acc x -> g x + acc) 0

prodWith''' g =foldl' (\acc x -> g x * acc) 1