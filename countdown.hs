-- countdown game

data Op = Add | Sub | Mul | Div 

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "÷"

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

valid :: Op -> Int -> Int -> Bool 
valid Add x y = x <= y           -- avoid symmetric duplicates
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0


data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n 
    show (App o l r) = brak l ++ show o ++ brak r 
                       where 
                           brak (Val n) = show n 
                           brak e = "(" ++ show e ++ ")"

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, 
                                  y <- eval r, 
                                  valid o x y]

pset :: [a] -> [[a]]

pset [] = [[]]
pset (x:xs) =  [l | l <- pset xs] ++ [x:l | l <- pset xs] 

--choices :: [a] -> [[a]]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r


-- build all permutations of a list
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : [ y:zs | zs <- interleave x ys ]

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = [ zs | ys <- perms xs, zs <- interleave x ys ]

-- choices = permutations of all subsets
choices :: [a] -> [[a]]
choices xs = [ zs | ys <- pset xs, zs <- perms ys ]

-- fix the type: needs the target Int
solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = elem (values e) (choices ns) && eval e == [n]


--solution :: Expr -> [Int] -> Bool 
--solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs)| (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, 
                                      r <- exprs rs,
                                      e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r| o <- [Add, Sub, Mul, Div]]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
