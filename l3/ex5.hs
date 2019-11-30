import Data.List
--1
sortDesc :: Ord a => [a] -> [a]
sortDesc x = (reverse . sort) x
--2
sortDesc2 x = reverse (sort x)
--3
w3 = \x y z -> sqrt (x^2 + y^2 + z^2)
--(f . w3 1 2 . h) 3

--4
are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t]-> Bool 
are2FunsEqAt f g = all (uncurry (==)) . map (\x -> (f x, g x))
--5
infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g
--6
composeFunList :: [a -> a] -> (a -> a)
composeFunList (f:fs) = f . (composeFunList fs)
--7
((.).(.)) (\x -> 2 * x) (\x y -> x + y) 2 3
