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

data Boolv2 = F | T 
   deriving (Show, Eq)

andv11 :: Boolv2 -> Boolv2 -> Boolv2 
andv11 F _ = F 
andv11 T b = b
