zipWith2 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith2 _ [] _ = []
zipWith2 _ _ [] = []
zipWith2 f (x:xs) (y:ys) = f x y : zipWith2 f xs ys

zip2 :: [a] -> [b] -> [(a,b)] 
zip2 _ [] = [] 
zip2 [] _ = [] 
zip2 (x:xs) (y:ys) = (x,y) : zip2 xs ys

myfun :: (Num n) => n -> n
myfun x = (*) (2) x

fun :: (Num a) => [a] -> a
fun x = loop 1 x
 where loop acc [] = acc
       loop acc (x:xs) = loop (x*acc) xs

sumAbs :: Num a => [a] -> a
sumAbs [] = 0
sumAbs (x:xs) = abs x + sumAbs xs

curry2 :: ((a,b) -> c) -> a -> b -> c
curry2 f a b = f (a,b)

uncurry2 :: (a -> b -> c) -> (a,b) -> c
uncurry2 f (a,b) = f a b

addT :: Num a => (a, a) -> a 
addT (x,y) = x + y

addC :: Num a => a -> a -> a 
addC x y = x + y

func :: Integral t => [t] -> t
func x = loop 0 x
 where loop acc [] = acc
       loop acc (x:xs) = loop (((^) (is3 x) 2) + acc) xs 
        where is3 x | x`mod`3 == 0 = x | otherwise = 0
 