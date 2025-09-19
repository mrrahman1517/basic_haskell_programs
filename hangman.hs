import MyIO
import System.IO

hangman :: IO ()
hangman = do myputStrLn "Think of a word: "
             word <- mysgetLine
             myputStrLn "Try to guess it:"
             play word 

mysgetLine :: IO String 
mysgetLine = do x <- mygetCh 
                if x == '\n' then 
                    do putChar x
                       return []
                else 
                    do putChar '-'
                       xs <- mysgetLine 
                       return (x:xs)

mygetCh :: IO Char 
mygetCh = do hSetEcho stdin False
             x <- getChar 
             hSetEcho stdin True 
             return x

play :: String -> IO ()
play word = 
    do myputStr "? "
       guess <- mygetLine
       if guess == word then 
          myputStrLn "You got it!"
       else 
          do myputStrLn (match word guess)
             play word 

match :: String -> String -> String 
match xs ys = 
    [if elem x ys then x else '-'| x <- xs]