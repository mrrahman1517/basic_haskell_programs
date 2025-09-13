-- interactive programming

act :: IO (Char, Char, Char)

act = do x <- getChar 
         y <- getChar
         z <- getChar 
         return (x, y, z)

mygetLine :: IO String 
mygetLine = do x <- getChar 
               if x == '\n' then 
                 return []
               else 
                 do xs <- mygetLine 
                    return (x:xs)