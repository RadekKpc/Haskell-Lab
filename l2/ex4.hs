isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s

getElemAtIdx :: Int -> [a] -> a
getElemAtIdx a x = x!!a

getElemAtIdx2 :: Int -> [a] -> a
getElemAtIdx2 a x =head(drop (a) x)

capitalize :: [Char] -> [Char]
capitalize w = toEnum(fromEnum(head w) - 32) : tail w





