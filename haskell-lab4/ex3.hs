import Data.List

data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a) 

instance Eq a => Eq (BinTree a) where
    EmptyBT==EmptyBT = True
    EmptyBT == _ = False
    _ == EmptyBT = False
    (NodeBT x lt rt) == (NodeBT y ylt yrt) = x==y && lt==ylt && rt==yrt

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | 
              Add (Expr a) (Expr a) |
              Substract (Expr a) (Expr a)|
              Product (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Substract e1 e2) = eval e1 - eval e2
eval (Product e1 e2) = (eval e1)*(eval e2)


show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Substract e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Product e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1+ depthOfBT lt

flattenBTin :: BinTree a -> [a]
flattenBTin EmptyBT = []
flattenBTin (NodeBT n lt rt) = (flattenBTin lt) ++ [n] ++ (flattenBTin rt)

flattenBTpre :: BinTree a -> [a]
flattenBTpre EmptyBT = []
flattenBTpre (NodeBT n lt rt) = [n] ++ (flattenBTpre lt) ++ (flattenBTpre rt)

flattenBTpost :: BinTree a -> [a]
flattenBTpost EmptyBT = []
flattenBTpost (NodeBT n lt rt) = (flattenBTpost lt) ++ (flattenBTpost rt) ++ [n]



mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT n' lt' rt'
    where
     n' = f n
     lt' = mapBT f lt
     rt' = mapBT f rt

insert' :: Ord a => a -> BinTree a -> BinTree a
insert' x EmptyBT = (NodeBT x EmptyBT EmptyBT)
insert' x (NodeBT n lt rt)
    | x==n = NodeBT x lt rt
    | x<n = NodeBT n (insert' x lt) rt 
    | x>n = NodeBT n lt (insert' x rt)



list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST [x] = NodeBT x EmptyBT EmptyBT
list2BST list = NodeBT x (list2BST lt) (list2BST rt)
                where
                    m=length list `div` 2
                    x = (sort list) !! m
                    lt = take m (sort list)
                    rt = drop (m+1) (sort list)
