isPalindrome :: [Char]->Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs | (head xs)==(last xs)=isPalindrome (init(tail xs))
                | otherwise = False

getElemAtIdx :: Int->[Char]->Char
getElemAtIdx n xs= last(take (n+1) xs)

