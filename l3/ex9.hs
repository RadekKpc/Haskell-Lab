sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs
   
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0
prodWith'' g = foldr' (\x acc -> g x * acc) 1

foldl2' :: (b -> a -> b) -> b -> [a] -> b
foldl2' f z [] = z
foldl2' f acc (x:xs) = foldl2' f (f acc x) xs 

sumWith''' g  = foldl2' (\acc x -> g x + acc) 0
prodWith''' g = foldl2' (\acc x -> g x * acc) 1

--3
--Main Data.Char> foldr' (+) 0 [1..10^6]
--500000500000
--(0.55 secs, 266,920,832 bytes)
--Main Data.Char> foldr (+) 0 [1..10^6]
--500000500000
--(0.21 secs, 161,586,544 bytes)

--4
--Main Data.Char> foldr (\x acc -> x + 1 + acc) 0 [1..10^6]
--500001500000
--(1.00 secs, 282,110,824 bytes)
--Main Data.Char> foldl (\acc x -> x + 1 + acc) 0 [1..10^6]
--500001500000
--(0.94 secs, 290,110,800 bytes)
--Main Data.Char Data.List> sum . map (+1) $ [1..10^6]
--500001500000
--(0.45 secs, 265,689,744 bytes)
--Main Data.Char Data.List> sum [x + 1 | x <- [1..10^6]]
--500001500000
--(0.95 secs, 353,689,512 bytes)

--7
map' g (x:xs) = foldr (\x acc -> g x : acc) [] (x:xs)
map'' g (x:xs) = foldl (\acc x -> g x : acc) [] (x:xs)

filter' g (x:xs) = foldr (\x acc -> if g x then x:acc else acc) [] (x:xs)
filter'' g (x:xs) = foldl (\acc x -> if g x then x:acc else acc) [] (x:xs)

