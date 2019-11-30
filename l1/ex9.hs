roots :: (Double,Double,Double) -> Double
roots (a,b,c) = d/c
  where d = a + b
  
unitVec2D :: (Double,Double) -> (Double,Double)
unitVec2D (x,y) = (x/n,y/n)
  where n = sqrt((x*x) + (y*y))
        