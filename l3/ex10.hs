--Desc and Asc
isSorted :: Ord a => [a] -> Bool
isSorted xs = (go xs) || (go (reverse xs))
 where
 go xs = all (==True) . zipWith (<=) (init xs) $ (tail xs)
 
everySecond :: [t] -> [t]
everySecond [] = []
everySecond [x] = [x]
everySecond (x:y:xs) = x : everySecond xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3' _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs

uncurry3' (a,b,c) 1 = a
uncurry3' (a,b,c) 2 = b
uncurry3' (a,b,c) 3 = c

unzip3' :: [(a, b, c)] -> ([a], [b], [c])
unzip3' [] = ([], [], [])
unzip3' ((a,b,c):xs) = go ([a],[b],[c]) (unzip3' xs)
 where go (a,b,c) (a2,b2,c2) = (a ++ a2, b ++ b2 , c ++ c2)
