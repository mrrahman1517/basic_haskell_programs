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

-- property 
-- boxs . boxs = id
-- ghci> boxs (boxs m) == m
--["123456789","123456789","123456789","123456789","123456789","123456789","123456789","123456789","123456789"]
boxs :: Matrix a -> [Row a]
boxs m = [concat(map (take 3. drop c) (take 3 (drop r m)))| r <- [0,3,6], c <- [0,3,6]]

-- nodups [1,2,3] = true
-- nodups [1,1,2] = false
--nodups :: [a] -> Bool 
--nodups [x] = True
--nodups (x:xs) = 


valid :: Grid -> Bool 
valid g = myall nodups (rows g) && myall nodups (cols g) && myall nodups (boxs g)

myall :: (a-> Bool) -> [a] -> Bool
-- all even [2,4,6] = true
myall p xs = and [p x | x <-xs]

nodups :: Eq a => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (elem x xs) && nodups xs


-- a basic solver

solve :: Grid -> [Grid]

solve = filter valid . collapse . choices 

--solve g = filter valid (collapse (choices g)) 

-- making choices
-- replace each blank with all choices 1-9

type Choices = [Value]
choices :: Grid -> Matrix Choices
choices g = map (map choice) g
            where choice v = if v == '.' then 
                                         ['1'..'9']
                             else 
                                [v]


prune :: Matrix Choices -> Matrix Choices 

prune = pruneBy boxs . pruneBy cols . pruneBy rows
        where pruneBy f = f . map reduce . f 

--ghci> map single ["1234","1","34","3"]
--[False,True,False,True]
single :: Choices -> Bool
single c = length c == 1

--ssss
fixed :: Row Choices -> Choices 
fixed r = concat [c| c <- r, single c]
-- fixed r = [ch | [ch] <- r]


dropAll forbidden s = [ch | ch <- s, notElem ch forbidden] 

step :: Choices -> Choices -> Choices
step f c = if single c 
            then c
           else dropAll f c  

reduce :: Row Choices -> Row Choices 
reduce r = let f = fixed r 
           in map (step f) r  

cp :: [[a]] -> [[a]]
cp = sequence

collapse :: Matrix [a] -> [Matrix a]
--collapse m = cp (map cp m)
collapse = sequence . map sequence

-- prune the search space

solve2 :: Grid -> [Grid] 
solve2 = filter valid . collapse . prune . choices

solve3 = filter valid . collapse . fix prune . choices 

-- fix point of a function f(x) = x, then x is a fix point 
fix :: Eq a => (a->a) -> a -> a
fix f x = if x == x' then x else fix f x'
          where x' = f x






