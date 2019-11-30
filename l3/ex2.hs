--1.
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Floating a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = (sqrt x) + sumSqr' xs
--2.
sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = (f x) + sumWith f xs
--3.
sum = sumWith (\x -> x)
sumSqr = sumWith (\x -> x*x)
sumCube = sumWith (\x -> x^3)
sumAbs = sumWith abs

--4. sumWith (\x -> x^5) [1..10]
--5.
listLength = sumWith (\x->1)

--6.

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * (prod' xs)

--7.

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = (f x) * prodWith f xs

--8 
sthWith :: Num a => (a->a) -> (a->a->a) -> [a] -> a -> a
sthWith f g [] h = h
sthWith f g (x:xs) h = g (f x) (sthWith f g xs h)
--9
sumSqrt = sumWith sqrt
prodSqrt = prodWith sqrt
