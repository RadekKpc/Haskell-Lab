--[(a,b,c) | a <- [1..100], b <- [a..100], c <- [b..100], a ^ 2 + b ^ 2 == c ^ 2]
isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []

-- length [i | i<- [1..1000], isPrime i]

primes :: [Int]
primes = eratoSieve [2..]
 where
  eratoSieve :: [Int] -> [Int]
  eratoSieve (p: xs) = p: eratoSieve [x | x <- xs,x `mod` p /= 0]
  
isPrime2 :: Int -> Bool
isPrime2 x = x `elem` (take x primes)

howmanyprimes :: Int -> Int
howmanyprimes n = length [i | i <- [1..n] , isPrime2 i]

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs
            


