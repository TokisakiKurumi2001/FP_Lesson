import System.IO

hangman :: IO ()
hangman = do
  putStrLn "Think of a word: "
  word <- sgetline
  putStrLn "Try to guess it: "
  --play word

-- getCh : receive a char from keyword but don't display it to the screen
-- getChar: do 3 things, receive char, display char to screen, return the value of char

{-
sgetline :: IO String
sgetline = do
  x <- getCh
  if x == '\n' then do
    putChar x
	return []
  else do
    putChar '*'
    xs <- sgetline
    return (x:xs)
-}

sgetLine :: IO String
sgetLine = do x <- getCh
  if x == '\n' then do
    putChar x
	return []
  else
  do putChar '-'
  xs <- sgetLine return (x:xs)
{-
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x
-}

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word then
    putStrLn "You got it!"
  else do
    putStrLn (match word guess)
	play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]