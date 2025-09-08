data Maybe1 a = MNothing | MJust a 
   deriving (Show)

data Shape = Circle Float 
           | Rect Float Float 
           deriving (Show)

data Boolv2 = F | T 
   deriving (Show, Eq)

data Answer = Y | N | U 
   deriving (Show, Eq)


type Stringv2 = [Char]

lengthv6 :: Stringv2 -> Int 
lengthv6 [] = 0
lengthv6 (x:xs) = 1 + lengthv6 xs 

type Pos = (Int, Int) 

origin :: Pos 
origin = (0,0)

left :: Pos -> Pos 
left (x,y) = (x-1,y)

right :: Pos -> Pos 
right (x,y) = (x+1, y) 

up :: Pos -> Pos 
up (x, y) = (x, y+1)

down :: Pos -> Pos 
down (x, y) = (x, y-1)

type Pair a = (a,a)

mult :: Pair Int -> Int 
mult (m,n) = m * n 

copy :: a -> Pair a 
copy x = (x, x)

-- type Pos = (Int, Int)
type Trans = Pos -> Pos
-- type Tree = (Int, [Tree]) , XX types cannot be declared recursively  

-- data declaration


andv11 :: Boolv2 -> Boolv2 -> Boolv2 
andv11 F _ = F 
andv11 T b = b

flip1 :: Answer -> Answer
flip1 Y = N 
flip1 N = Y 
flip1 U = U

square :: Float -> Shape 
square n = Rect n n 

area :: Shape -> Float 
area (Circle r) = pi * r^2 
area (Rect x y) = x * y

safediv ::Int -> Int -> Maybe1 Int 
safediv _ 0 = MNothing
safediv m n = MJust (m `div` n)

safehead :: [a] -> Maybe1 a 
safehead [] = MNothing
safehead (x:xs) = MJust (x) 

-- recursive types 

data Nat1 = Zero1 | Succ Nat1
   deriving (Show)   

toInt :: Nat1 -> Int 
toInt Zero1 = 0
toInt (Succ n) = 1 + toInt n 

nat2int :: Nat1 -> Int 
nat2int Zero1 = 0
nat2int (Succ n) = 1 + nat2int n 

int2nat :: Int -> Nat1
int2nat 0 = Zero1
int2nat n = Succ (int2nat (n-1))

addNat1 :: Nat1 -> Nat1 -> Nat1 
addNat1 m n = int2nat (nat2int m + nat2int n)

addNat2 :: Nat1 -> Nat1 -> Nat1 
addNat2 Zero1 n = n 
addNat2 (Succ m) n = Succ (addNat2 m n)

multNat1 :: Nat1 -> Nat1 -> Nat1 
multNat1 m n = int2nat (nat2int m * nat2int n)

multNat2 :: Nat1 -> Nat1 -> Nat1 
multNat2 Zero1 n = Zero1
multNat2 (Succ m) n = addNat2 (multNat2 m n) n 

-- arithmetic expressions
data Expr = Val Int 
          | Add Expr Expr 
          | Mul Expr Expr
          deriving (Show) 

-- binary tree
--data TreeNode = Val Int | [TreeNode left, TreeNode right]
data Tree1 a = Leaf a 
             | Node (Tree1 a) (Tree1 a)

data Tree2 a = Leaf2 a 
             | Node2 a (Tree2 a) (Tree2 a)

rootValue2 :: Tree2 a -> a 
rootValue2 (Leaf2 x) = x 
rootValue2 (Node2 x _ _) = x 

treeSize2 :: Tree2 a -> Int 
treeSize2 (Leaf2 x) = 1
treeSize2 (Node2 _ left right) = 1 + treeSize2 left + treeSize2 right

t2 :: Tree2 Int 
t2 = Node2 1 
        (Node2 2 
              (Leaf2 3) (Leaf2 4)) 
        (Node2 5 
              (Leaf2 6) (Leaf2 7))  



treexpr1 :: Tree1 Int
treexpr1 = Leaf 5

treexpr2 :: Tree1 Int 
treexpr2 = Node (Leaf 6) (Leaf 7)

expr1 :: Expr
expr1 = Add (Val 2) (Val 3)

expr2 :: Expr 
expr2 = Add (Val 2) (Mul (Val 3) (Val 4))

eval :: Expr -> Int 
eval (Val n) = n 
eval (Add x y) = eval x + eval y 
eval (Mul x y) = eval x * eval y

size :: Expr -> Int 
size (Val n) = 1 
size (Add x y) = size x + size y 
size (Mul x y) = size x + size y