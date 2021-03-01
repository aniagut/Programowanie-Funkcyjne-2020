newtype Box a = MkBox a deriving Show

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> w = fmap f w

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)
  
newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
    fmap f (MyTriple (a,b,c)) = MyTriple ((f a), (f b), (f c))

instance Applicative MyTriple where
    pure x = MyTriple (x, x, x)
    MyTriple (f, g, h) <*> MyTriple (x, y, z) = MyTriple ((f x), (g y), (h z))