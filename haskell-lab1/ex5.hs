sgn :: Int -> Int
sgn n= if n<0
    then -1
    else if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt n= if n<0
    then -n
    else n

min2Int :: (Int, Int) -> Int
min2Int (x,y)= if x<y
    then x
    else y

min3Int :: (Int, Int, Int)->Int
min3Int (x,y,z)=if x<y
    then if x<z
        then x
        else z
    else if y<z
        then y
        else z

min3Int2 :: (Int, Int, Int)->Int
min3Int2 (x,y,z)=min2Int(min2Int(x,y),z)

toUpper :: Char -> Char
toUpper a= toEnum((fromEnum a) -32)

toLower :: Char -> Char
toLower a= toEnum((fromEnum a) +32)

isDigit ::Char -> Bool
isDigit a= if (fromEnum a)>=(fromEnum '0') && (fromEnum a)<=(fromEnum '9')
    then True
    else False




 
