not' :: Bool->Bool
not' b = case b of
    True -> False
    False -> True

absInt n =
    case (n>=0) of
        True -> n
        _ -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer a = case a of
    "Love" -> True
    _ -> False

or' :: (Bool,Bool)->Bool
or' q = case q of
    (False,False) -> False
    _ -> True

and' :: (Bool,Bool) -> Bool
and' q = case q of
    (True,True) -> True
    _ -> False

nand' :: (Bool,Bool) -> Bool
nand' q=case q of
    (True,True) -> False
    _ -> True

xor' :: (Bool,Bool)-> Bool
xor' q = case q of
    (True,False) ->True
    (False,True) ->True
    _ -> False


