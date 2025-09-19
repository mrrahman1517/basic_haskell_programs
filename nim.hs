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

valid :: Board -> Int -> Int -> Bool 
valid b row num = b !! (row-1) >= num

-- assume move is valid using previous check
--move :: Board -> Int -> Int -> Board
--move b row num = [adjust r n | (r,n) <- zip [1..5] b]
--                 where
--                    adjust r n = if r == row then n-num else n 

-- assume move is valid using previous check
move :: Board -> Int -> Int -> Board
move b row num =
  map (\(r, n) -> if r == row then n - num else n) (zip [1..5] b)

-- I/O utilities

newline :: IO ()
newline = putChar '\n'

stars :: Int -> String 
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (stars num)

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a 
                          putRow 2 b
                          putRow 3 c 
                          putRow 4 d 
                          putRow 5 e

getDigit :: String -> IO Int 

getDigit prompt = do putStr prompt 
                     x <- getChar 
                     newline 
                     if isDigit x then 
                        return (digitToInt x) 
                     else 
                        do newline 
                           putStrLn "Error: invalid digit!"
                           getDigit prompt
