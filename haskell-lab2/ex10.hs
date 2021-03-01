fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

f1 :: [Int] -> Bool
f1 (x : y : _) | (y `mod` x) == 0 = True
f1 _                                = False

f2 :: [Int] -> Bool
f2 (x:y:z:_) | (z `mod` x) == 0 = True
f2 _                            = False