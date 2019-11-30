not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True 
isItTheAnswer _      = False

or' :: (Bool,Bool) -> Bool
or' (x,y) = x || y
and' :: (Bool,Bool) -> Bool
and' (c,y) = c && y
nand' :: (Bool,Bool) -> Bool
nand' (a,b) = not' (and' (a,b))



