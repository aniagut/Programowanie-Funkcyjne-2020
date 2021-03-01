sqr x = x^2

funcFactory n = case n of
    1 -> id
    2 -> sqr
    3 -> (^3)
    4 -> \x -> x^4
    5 -> intFunc
    _ -> const n
    where
        intFunc x = x^5


prod' :: Num a=> [a] -> a
prod' [] = 1
prod' (x:xs) = x* prod' xs

expApproxUpTo :: Int -> Double -> Double
expApproxUpTo n = case n of
    0 ->  const 1
    1 ->  (\x -> 1 + (x / (fact 1)))
    2 ->  (\x -> (expApproxUpTo 1 x) + ((x^2) / (fact 2)))
    3 ->  (\x -> (expApproxUpTo 2 x) + ((x^3) / (fact 3)))
    4 ->  (\x -> (expApproxUpTo 3 x) + ((x^4) / (fact 4)))
    5 ->  (\x -> (expApproxUpTo 4 x) + ((x^5) / (fact 5)))
    6 ->  (\x -> (expApproxUpTo 5 x) + ((x^6) / (fact 6)))
    y -> (\x -> (expApproxUpTo (y-1) x) + ((x^y) / (fact y)))
    where
        fact n = fromIntegral $ product [1..n]







