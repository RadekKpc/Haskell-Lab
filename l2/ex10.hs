fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fst2div :: Integral a => [a] -> Bool
fst2div (x : y : _) | (y `mod` x) == 0  = True
fst2div _                    = False