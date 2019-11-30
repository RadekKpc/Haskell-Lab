--1
funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs
--2
displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)
--3.1
funcListExt :: [Double -> Double]
funcListExt = funcList ++ [\x -> sqrt (1 + x )]
--3.2
--From ex3.hs
dfr :: (Double -> Double) -> Double -> (Double -> Double)
dfr f h = (\x -> (/) ((-)(f (x + h)) (f (x - h))) (2*h))

d2f :: (Double -> Double) -> Double -> (Double -> Double)
d2f f h = dfr (dfr f h) h

velocEqs :: (Double -> Double, Double -> Double) -> Double ->(Double, Double)
velocEqs (fx,fy) t = (dfr fx 100 t,dfr fy 100 t)

accelEqs :: (Double -> Double, Double -> Double) -> Double ->(Double, Double)
accelEqs (fx,fy) t = (d2f fx 100 t,d2f fy 100 t)

position = ((\x -> 2*x*x + 6*x + 2) :: Double -> Double,(\x -> 2*x*x + 2*x + 1):: Double -> Double)
