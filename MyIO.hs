module MyIO (
  act,
  mygetLine,
  myputStr,
  myputStrLn,
  mystrlen,
  showstrlen
) where

-- interactive programming

act :: IO (Char, Char, Char)
act = do
  x <- getChar
  y <- getChar
  z <- getChar
  return (x, y, z)

mygetLine :: IO String
mygetLine = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- mygetLine
      return (x : xs)

myputStr :: String -> IO ()
myputStr [] = return ()
myputStr (x:xs) = do
  putChar x
  myputStr xs

myputStrLn :: String -> IO ()
myputStrLn xs = do
  myputStr xs
  putChar '\n'

mystrlen :: String -> Int
mystrlen []     = 0
mystrlen (_:xs) = 1 + mystrlen xs

showstrlen :: IO ()
showstrlen = do
  myputStr "Enter a string: "
  xs <- mygetLine
  myputStr "The string has "
  myputStr (show (mystrlen xs))
  myputStrLn " chars"
