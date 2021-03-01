data Foo a = MkFoo {value:: a, name::String}
instance Show a => Show (Foo a) where
    show (MkFoo{value=v, name=n}) = "Foo " ++ show n ++ " with " ++ show v


data Tree a = Node (Tree a) a (Tree a)
            | Leaf a

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node left x right) = toList left ++ [x] ++ toList right

data Data = Data {first :: String, second:: String}

data Tree' a = Node' (Tree' a) a (Tree' a)
             | Leaf'

sumSq :: Num a => Tree' a -> a
sumSq Leaf' = 0
sumSq (Node' left x right) = x^2 + sumSq left + sumSq right

newtype Box a = MkBox {valueInside :: a}

instance Show a => Show (Box a) where
    show (MkBox{valueInside=v}) = "Box with " ++ show v


