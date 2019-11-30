qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<x) xs
   rightPart xs = filter (>=x) xs

mSort :: Ord a => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = merge (mSort left) (mSort right)
 where n = length xs `div` 2
       (left, right) = splitAt n xs
       merge [] ys = ys
       merge xs [] = xs
       merge (x:xs) (y:ys)
                     | x < y = x : merge xs (y:ys)
                     | otherwise = y : merge (x:xs) ys
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort [x] = [x]
iSort (x:xs) = insert x (iSort xs)
 where insert x [] = [x]
       insert n (x:xs) | x > n = n:x:xs
                       | otherwise = x : (insert n xs)

concat' :: [[a]] -> [a]
concat' xs = [ i | x <- xs, i<- x]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ (concat'' xs)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted (x:xs) = (all (>=x) xs) && isSorted(xs)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [x] = [x]
reverse' (x:xs) = (reverse' xs) ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [x] [] = []
zip' [] [x] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ (zip' xs ys)

unzip' :: [(a,b)] -> ([a],[b])
unzip' = loop [] []
         where 
          loop acc1 acc2 [] = (acc1,acc2)
          loop acc1 acc2 ((x, y):xs) = loop (acc1 ++ [x]) (acc2 ++ [y]) xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' [] _ _ = []
zip3' _ [] _ = []
zip3'  _ _ [] = []
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z):(zip3' xs ys zs)

subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True
subList (x:xs) ys = x `elem` ys && subList xs ys
