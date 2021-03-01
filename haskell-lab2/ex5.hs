--1.ghci> length([(a,b,c) | a<- [1..100], b<-[1..100], c<-[1..100], a^2+b^2==c^2])
isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []
--poprawna,nieefektywna
