{-# LANGUAGE DeriveFunctor #-}
newtype Box a = MkBox a deriving (Show, Functor)

--instance Functor Box where
  --fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList
              | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)


data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
    fmap _ EmptyBT = EmptyBT
    fmap f (NodeBT a lt rt) = NodeBT (f a) (fmap f lt) (fmap f rt)
    