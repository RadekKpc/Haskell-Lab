sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

 --1
expApproxUpTo :: Int -> Double -> Double
expApproxUpTo 0 = (\x -> 1)
expApproxUpTo n = (\x -> (/) (funcFactory (fromIntegral n) x) (fromIntegral (product [1..n])) + expApproxUpTo (n-1) x)

--2
funFactory2 :: Int -> Double -> Double
funFactory2 n = (\x -> x^n)

expApproxUpTo2 :: Int -> Double -> Double
expApproxUpTo2 0 = (\x -> 1)
expApproxUpTo2 n = (\x -> (/) (funFactory2 n x) (fromIntegral (product [1..n])) + expApproxUpTo2 (n-1) x)

dfc :: (Double -> Double) -> Double -> (Double -> Double)
dfc f h = (\x -> (/) ((-) (f (x + h)) (f (x))) h )