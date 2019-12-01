concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

concat'' x = foldr (++) [] x

concat''' x = [b | a<-x , b<-a]

--2
concat . map (\x -> [2*x]) $ [1..5]
concatMap (\x -> [2*x]) [1..5]
concatMap (\x -> concat [x ++ "!"]) ["Ready", "Steady", "Go"]