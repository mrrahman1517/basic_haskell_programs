--sudoku game solver

-- basic type declarations

import Data.List (transpose)

type Grid = Matrix Value 

type Matrix a = [Row a]

type Row a = [a]

type Value = Char 

-- type Grid = [[Char]]
-- the matrix, row and value types are also useful

--example
-- easy grid
easy :: Grid 
easy = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..81..2.",
        "152369784",
        "4..25...."]

-- empty grid 
-- replicate :: Int -> a -> [a]
blank :: Grid 
blank = replicate 9 (replicate 9 '.')

uniform :: Grid 
uniform = replicate 9 ("123456789") 

rows :: Matrix a -> [Row a]
--rows M = M
rows = id -- id x = x 

-- property or rows, rows (rows m) = m

-- property : rows . rows = id

cols :: Matrix a -> [Row a]
cols = transpose
-- cols . cols = id 

--boxs :: Matrix a -> [Row a]

slices3 :: [a] -> [[a]]
slices3 [] = []
slices3 xs = take 3 xs : slices3 (drop 3 xs)

m = uniform

---map (take 3. drop 0) (take 3(drop 0 m))
---["123","123","123"]
---ghci> map (take 3. drop 3) (take 3(drop 0 m))
---["456","456","456"]
---ghci> map (take 3. drop 6) (take 3(drop 0 m))
---["789","789","789"]

boxs :: Matrix a -> [Row a]
boxs m = [concat(map (take 3. drop c) (take 3 (drop r m)))| r <- [0,3,6], c <- [0,3,6]]







