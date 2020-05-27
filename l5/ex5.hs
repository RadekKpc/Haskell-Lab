actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >> putChar '\n'

doActSeq = do
 putChar 'A'
 putChar 'G'
 putChar 'H'
 putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
 line <- getLine
 putStrLn line
 
echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
 line <- getLine
 putStrLn $ line ++ "!"

echo3 :: IO()
echo3 = getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO()
dialog = putStr "Whats is your number ?"
 >> getLine
 >>= \n -> let num = read n :: Int in
  if num == 7
  then putStrLn "Luckyh 7"
  else if odd num
   then putStrLn "ASDAS"
   else putStrLn "As"

doEcho3 = do
 putStr "podaj mimie ?"
 n <- getLine
 let num = read n :: Int in
  if num == 7
  then putStrLn "Luckyh 7"
  else if odd num
   then putStrLn "ASDAS"
   else putStrLn "As"

doDialog = do
 l1 <- getLine
 l2 <- getLine
 putStrLn $ l1 ++ l2

twoQuestions :: IO()
twoQuestions = putStrLn "NAME?" >> getLine >>= \n -> putStrLn "age ?" >> getLine >>= \a -> print (n,a)


