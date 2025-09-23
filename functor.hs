
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
    fmap :: (a->b)-> f a -> f b

-- list functor

instance MFunctor [] where 
    --fmap :: (a->b)->[a]->[b]
    fmap = map

-- the maybe functor 
data MMaybe a = MNothing | MJust a 

instance MFunctor MMaybe where
    --fmap :: (a->b)-> MMaybe a -> MMaybe b 
    fmap g MNothing = MNothing
    fmap g (MJust x) = MJust (g x)