-- game of nim

-- getLine :: IO String
-- putStr :: String -> IO ()
-- putStrLn :: String -> IO ()

-- do for sequencing actions

import Data.Char

-- board utilities:

type Board = [Int]

initial :: Board

initial = [5,4,3,2,1]

finished :: Board -> Bool 
--finished [x] = x == 0
--finished (b:bs) = b == 0 && finished bs
finished b = all (==0) b
