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