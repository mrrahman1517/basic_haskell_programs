
-- abstracting programming patterns

inc :: [Int] -> [Int]
--inc [] = []
--inc (n:ns) = n+1 : inc ns 
inc  = mmap (+1)

sqr :: [Int] -> [Int]
--sqr [] = []
--sqr (n:ns) = n^2 : sqr ns
sqr = mmap (^2)

mmap :: (a -> b) -> [a] -> [b]
mmap f [] = []
mmap f (x:xs) = f x : mmap f xs 

-- generalize further
-- functor replace list with any data structure

-- f is a parameterized type such as [a], tree a, graph a, etc
class MFunctor f where 
    mfmap :: (a->b)-> f a -> f b

-- list functor

instance MFunctor [] where 
    --fmap :: (a->b)->[a]->[b]
    mfmap = map

-- the maybe functor 
data MMaybe a = MNothing | MJust a 

instance MFunctor MMaybe where
    --fmap :: (a->b)-> MMaybe a -> MMaybe b 
    mfmap g MNothing = MNothing
    mfmap g (MJust x) = MJust (g x)

-- tree functor 

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving Show

t1 :: Tree String 
t1 = Node (Leaf "a") (Leaf "bc")

t2 :: Tree String 
t2 = Node (Leaf "def") (Leaf "ghij")

root :: Tree String 
root = Node t1 t2

tree_length :: Tree a -> Int
tree_length (Leaf _) = 1
tree_length (Node left right) = 1 + tree_length left + tree_length right 

instance Functor Tree where 
    -- fmap :: (a -> b) -> Tree a -> Tree b 
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

    -- g :: a->b
    -- x :: a
    -- l,r :: Tree a

--ghci> fmap (even . length) root
--Node (Node (Leaf False) (Leaf True)) (Node (Leaf False) (Leaf True))
--ghci> root
--Node (Node (Leaf "a") (Leaf "bc")) (Node (Leaf "def") (Leaf "ghij"))     


myinc :: Functor f => f Int -> f Int 
myinc = fmap (+1)

--ghci> (myinc.myinc) (Node (Leaf 1) (Leaf 2))
--Node (Leaf 3) (Leaf 4)

-- fmap0 :: a -> f a
-- fmap1 :: (a-b)-> fa -> fb
-- fmap2 :: (a->b->c)-> fa -> fb -> fc
-- fmap3 :: (a->b->c->d)->f a -> f b -> f c -> f d