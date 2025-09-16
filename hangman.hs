import MyIO

hangman :: IO ()
hangman = do myputStrLn "Think of a word: "
             word <- mysgetLine
             myputStrLn "Try to guess it:"
             play word 