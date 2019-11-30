sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt((+) (sqr x) (sqr y))


vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x,y,z) =  sqrt ((+) (sqr x) (sqr y) + (sqr z))

swap :: (Int, Char) -> (Char, Int)
swap (x,y) = (y,x)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x,y,z) = (x == y) && (y == z)

heron :: (Double,Double,Double) -> Double
heron (x,y,z) = (sqrt((x+y+z)*(x+y-z)*(x-y+z)*(-x+y+z)))/4

f2 (x,y,z) = if (x > y) then (x,y,z) else (y,x,z)

swap2(x,y) = (y,z) where z = x
f3 x = if x > 3 then 2 * x else x / 42

sgn :: Int -> Int 
sgn n | n < 0 = -1 | n == 0 = 0 | otherwise = 1

sgn2 :: Int -> Int
sgn2 n = if n < 0 
         then n
         else if n == 0
              then 0
              else -n

absInt2 :: Int -> Int
absInt2 x = if x < 0 
            then -x
            else x