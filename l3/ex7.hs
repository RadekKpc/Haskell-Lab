--1
onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs
 
onlyOdd [] = []
onlyOdd (x:xs)
 | x `mod` 2 == 1 = x : onlyOdd xs
 | otherwise      = onlyOdd xs
 --2
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
 | p x = x : filter' p xs
 | otherwise = filter' p xs
 
onlyOdd2 = filter' (\x -> x `mod` 2 == 1)
onlyEven2 = filter' (\x -> x `mod` 2 == 0)
--4
--length . filter even  $[1..10^6]
--5
--length (filter even [1..10^6]) 
--length [a | a <- [1..10^6] , even a]