min2Int :: (Int,Int) -> Int
min2Int (x,y) | x < y = x| otherwise =y

min3Int :: (Int,Int,Int) ->Int
min3Int (x,y,z) = min2Int(min2Int(x,y),z)

toUpper :: Char -> Char
toUpper x = toEnum(fromEnum(x) + 32)

charToNum :: Char -> Int
charToNum x | x=='0' =0| x=='1' =1| x=='2' =2| otherwise =0

absInt :: Int -> Int
absInt n | n <0 = -n | n>0 = n | otherwise =0

min33Int :: (Int, Int, Int) -> Int
min33Int (x,y,z) | (x<=y && x<=z) =x | (y<=x && y<=z) =y | otherwise =z
