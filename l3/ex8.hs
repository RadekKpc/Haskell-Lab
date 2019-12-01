--1
doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs
--2
sqrElems [] = []
sqrElems (x:xs) = x*x : sqrElems xs

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' = map (\x -> 2*x)
sqrElems' = map (\x -> x*x)

--List comprehensions
doubleElems'' (n:xn) = [2*a | a<-(n:xn)]
sqrElems'' (n:xn) = [a*a | a <-(n:xn)]
--4
--Main> length . doubleElems $ [1..1000000]
--1000000
--(0.32 secs, 256,065,392 bytes)
--Main> length . doubleElems' $ [1..1000000]
--1000000
--(0.03 secs, 160,065,312 bytes)
--Main> length . doubleElems'' $ [1..1000000]
--1000000
--(0.28 secs, 192,065,536 bytes)
--Main> length . filter even $ doubleElems [1..10^7]
--10000000
--(8.61 secs, 4,880,066,920 bytes)
--Main> length . filter even . map (*2) $ [1..10^7]
--10000000
--(2.51 secs, 3,360,067,032 bytes)

--7
evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x = map ($ x)
