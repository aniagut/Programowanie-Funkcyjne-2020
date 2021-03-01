--typy:
--Int - skonczonej precyzji
--Integer - nieskonczonej precyzji
--Double/Float
--Bool
--Char 'a'
--String [Char] "ala"
--(, ,)-tupla,krotka (Int,Char,Bool)
--[, ,] -lista [Int]
--not Bool->Bool - funkcja

--operatory:
-- a==b, a/=b, a<b, a>b, a<=b, a>=b, a&&b, a||b, not a
--a+b, a-b, a*b, a^b, a**b-to samo co potega,a/b, a`mod`b,
--a`div`b -dzielenie calkowite
--oba argumenty tego samego typu poza ** i ^
main=putStrLn "Hello,hello!"

--przykład funkcji
volume:: Double -> Double
volume r = 4/3*pi*r^3
--jezeli pominiemy typ to zostanie on wywnioskowany

--wyrażenie warunkowe
abs'::Int->Int
abs' n = if n >=0 then n else (-n)

sgn ::Int ->Int
sgn n = if n<0
    then -1
    else if n==0
        then 0
        else 1

--GUARDS
abs1:: Int->Int
abs1 n | n>=0 =n
      | otherwise = -n

--otherwise==True

--dopasowanie wzorców i wyrażenie case..of

not1:: Bool -> Bool
not1 True = False
not1 False = True

isTheName :: String -> Bool
isTheName "ABCDE" = True
isTheName _ = False
--_-wszystko inne

--case ... of

not2:: Bool->Bool
not2 b = case b of
    True -> False
    False ->True

--where

roots:: (Double,Double,Double)->(Double,Double)
roots (a,b,c) = ((-b-d)/e,(-b+d)/e)
    where d=sqrt(b*b-4*a*c)
          e=2*a
--mozna tez where {d=sqrt(b*b-4*a*c),e=2*a}
--let..in

roots1 :: (Double,Double,Double)->(Double,Double)
roots1 (a,b,c) =
    let d=sqrt(b*b-4*a*c)
        e=2*a
    in ((-b-d)/e,(-b+d)/e)

--where jest tylko lokalnie w wyrazeniu, nie poza nim

--where i let razem
f x = let a=10*x
    in a
    where a=100*x
--wynik: 40-definicje po let przyslaniaja te po where

--id :: a -> a

--(exp 1) -e^1
-- (+) 2 3

-- let e=exp 1

sqr::Double->Double
sqr x = x*x

swap1 :: (Int,Char)->(Char,Int)
swap1 (a,b) = (b,a)

min3Int:: (Int,Int,Int)->Int
min3Int (a,b,c)= if a<=b && a<=c
                then a
                else if b<=a && b<=c
                    then b
                    else c

min3Int1 :: (Int,Int,Int)->Int
min3Int1 (a,b,c) | a<=b && a<=c  =a
                 | b<=a && b<=c  =b
                 | otherwise =c


or' :: (Bool,Bool)->Bool
or' (False,False) = False
or' _ =True

or1 :: (Bool,Bool)->Bool
or1 (a,b) = case (a,b) of
            (False,False)->False
            _ ->True

unitVec2D :: (Double,Double)->(Double,Double)
unitVec2D (a,b) = (a/c,b/c)
    where c=sqrt(a^2+b^2)

unitVec2D1:: (Double,Double)->(Double,Double)
unitVec2D1 (a,b) = 
    let c=sqrt(a^2+b^2)
    in (a/c,b/c)

