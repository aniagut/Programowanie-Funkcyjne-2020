import Data.Char
import Data.List 
--type
type Name=String
capitalizeName :: Name->Name
capitalizeName = map toUpper
--ghci> capitalizeName "t-1000"

--newtype
newtype FirstName = FirstName String
formatFstName :: FirstName -> String
formatFstName (FirstName s) = case s of
    (x:xs) -> toUpper x : map toLower xs
    [] -> []

-- ghci >formatFstName (FirstName "apolinary") 

--Product type
data AandB a b =AandB_Con a b

data Person n s = Person n s
--Person :: n -> s -> Person n s

--p1=Person "Indigo" "Montoya"
--p1 :: Person [Char] [Char]

--Sum type
data ABorB a b=AB_Con a b | B_Con b 

data Either a b = Left a | Right b
--Left :: a -> Either a b
--Right :: b -> Either a b

--enum
data ThreeColors = Blue |
                   White|
                   Red 

--Blue :: ThreeColors        

--Algebraiczne typy danych - record syntax
data Person' = Person' String String 
               deriving (Show)
 
name :: Person' -> String
name (Person' n _) = n

surname :: Person' ->String
surname (Person' _ s) = s

--let sworMaster = Person' "abc" "efg"

--record syntax 

data Person'' = Person'' 
                { name' :: String
                , surname' :: String
                } deriving (Show)

--let swordMaster = Person'' { surname'="Montoya", name'="Indigo"}
--ghci >name' swordMaster

data Car = Car 
           { company:: String 
           , model :: String 
           ,year :: Int 
           } deriving (Show)
--or
data Car' a b c = Car'
                  { company' :: a 
                  , model' :: b 
                  , year' :: c 
                  } deriving (Show)

carInfo :: (Show a ) => Car' String String a -> String
carInfo (Car' {company'=c, model'=m,year'=y})=
    "This "++c ++" "++m++" was made in "++ show y


data Tree a = Nil |
              Node a (Tree a) (Tree a)
              deriving (Eq,Ord,Show,Read)

depth :: Tree a->Int 
depth Nil = 0
depth (Node n lt rt) = 1 + max (depth lt) (depth rt)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node n lt rt) = collapse lt ++ [n] ++ collapse rt

mapTree :: (a->b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node n lt rt) = Node (f n) (mapTree f lt) (mapTree f rt)

--klasy typów i ich instancje

newtype Vec2Dnt a = Vec2Dnt (a,a)
instance Eq a=> Eq (Vec2Dnt a) where 
    (==) (Vec2Dnt (x1,y1)) (Vec2Dnt(x2,y2)) = x1==x2 && y1==y2
--ghci >Vec2Dint (2,2)==Vec2Dint(2,2)

--abstrakcyjne typy danych


-- nie dziala module Stack

data Box a = MkBox {value:: a}
instance Eq a => Eq (Box a) where
    (==) (MkBox {value=v}) (MkBox {value=v'}) = v==v'

data Tree1 a = Node' (Tree1 a) a (Tree1 a)
            | Leaf 

sumSq :: Num a=> Tree1 a ->a
sumSq Leaf = 0
sumSq (Node' left x right) =x^2 + sumSq left +sumSq right
--(Node' (Node' Leaf 3 Leaf) 5 (Node' Leaf 6 Leaf))