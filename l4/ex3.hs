
data Tree a = Node [Tree a] a | Leaf a
maxValue :: Ord a => Tree a -> a
maxValue (Leaf a) = a
maxValue (Node [] x) = x
maxValue (Node (x:xs) b) = max (maxValue (Node xs b)) (maxValue x)