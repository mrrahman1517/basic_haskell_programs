{-|

Countdown Game Solver
====================

This program generates all possible arithmetic expressions from a list of numbers
using +, -, *, and ÷ to reach a target number, as in the classic Countdown game.

USAGE:
    1. Load in GHCi: :l countdown.hs
    2. Find all solutions: solutions [1,3,7,10,25,50] 765
    3. See all possible expressions: exprs [1,3,7]
    4. Check if an expression is a solution: solution (App Add (Val 3) (Val 7)) [3,7,10] 10

All functions are pure and can be used interactively in GHCi.
-}
-- countdown game

-- | Arithmetic operations for expressions


type Result = (Expr, Int)

data Op = Add | Sub | Mul | Div 

-- | Pretty-printing for Op
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "÷"


-- | Apply an operation to two integers
apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y
apply Sub x y = x - y 
apply Mul x y = x * y 
apply Div x y = x `div` y

--valid :: Op -> Int -> Int -> Bool 
--valid Add _ _ = True 
--valid Sub x y = x > y 
--valid Mul _ _ = True 
--valid Div x y = x `mod` y == 0 

-- | Check if an operation is valid for two integers (e.g., avoid division by zero, negative results, or duplicates)
valid :: Op -> Int -> Int -> Bool 
valid Add x y = x <= y           -- avoid symmetric duplicates
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y -- 1 * x = x
valid Div x y = y > 1 && x `mod` y == 0


-- | Arithmetic expression: either a value or an application of an operation
data Expr = Val Int | App Op Expr Expr

-- | Pretty-printing for expressions
instance Show Expr where
    show (Val n) = show n 
    show (App o l r) = brak l ++ show o ++ brak r 
                       where 
                           brak (Val n) = show n 
                           brak e = "(" ++ show e ++ ")"

-- | Evaluate an expression, returning all possible results (if valid)
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, 
                                  y <- eval r, 
                                  valid o x y]

-- | Power set: all subsets of a list
pset :: [a] -> [[a]]

pset [] = [[]]
pset (x:xs) =  [l | l <- pset xs] ++ [x:l | l <- pset xs] 

--choices :: [a] -> [[a]]

-- | Extract all values from an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r


-- build all permutations of a list
-- | Insert an element into every possible position in a list
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : [ y:zs | zs <- interleave x ys ]

-- | All permutations of a list
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [ zs | ys <- perms xs, zs <- interleave x ys ]

-- choices = permutations of all subsets
-- | All possible choices (permutations of all subsets) of a list
choices :: [a] -> [[a]]
choices xs = [ zs | ys <- pset xs, zs <- perms ys ]

-- fix the type: needs the target Int
-- | Check if an expression is a solution for the given numbers and target
solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = elem (values e) (choices ns) && eval e == [n]


--solution :: Expr -> [Int] -> Bool 
--solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- | All ways to split a list into two non-empty parts
split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs)| (ls,rs) <- split xs]

-- | All possible expressions that can be built from a list of numbers
exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, 
                                      r <- exprs rs,
                                      e <- combine l r]

-- | Combine two expressions with all operations
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r| o <- [Add, Sub, Mul, Div]]

-- | All solutions: expressions using the given numbers that evaluate to the target
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]


--results :: [Int] -> [Result]
--results ns = [(e,n) | e <- exprs ns, n <- eval e]

--combine' :: Result -> Result -> [Result]
--combine' lr rr = [(e,v)| o <- [Add, Sub, Mul, Div], 
--                    (l,_) <- lr, 
--                    (r,_) <- rr, 
--                    e <- App o l r, 
--                    v <- eval e]

--type Result = (Expr, Int)

results :: [Int] -> [Result]
results []     = []
results [n]    = [(Val n, n) | n > 0]
results ns     =
  [ res
  | (ls, rs) <- split ns
  , lres     <- results ls
  , rres     <- results rs
  , res      <- combine' lres rres
  ]

combine' :: Result -> Result -> [Result]
combine' (lExpr, lx) (rExpr, ry) =
  [ (App o lExpr rExpr, apply o lx ry)
  | o <- [Add, Sub, Mul, Div]
  , valid o lx ry
  ]

-- If you want a fast solver using the bottom-up results:
solutionsFast :: [Int] -> Int -> [Expr]
solutionsFast ns target =
  [ e
  | ns'     <- choices ns
  , (e, v)  <- results ns'
  , v == target
  ]
