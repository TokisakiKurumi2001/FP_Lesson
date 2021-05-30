act :: IO (Char, Char)
act = do 
  x <- getChar
  getChar
  y <- getChar
  return (x, y)

getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n' then
    return []
  else do
    xs <- getLine'
    return (x:xs)

lenInput :: IO ()
lenInput = do
  putStr "Enter a string: "
  str <- getLine
  putStr "The string has "
  putStr (show (length str))
  putStrLn " characters"

main = do
  {-
  (x, y) <- act
  print(x)
  print(y)
  ls <- getLine'
  print(ls)
  -}
  lenInput