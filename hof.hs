twice :: (a->a)->a->a
twice f x = f (f x)

sqr :: Int -> Int 
sqr x = x * x

plusv3 :: Int -> Int -> Int 
plusv3 x y = x + y

incv3 :: Int -> Int 
incv3 = plusv3 1

-- twice incv3 31
-- map (plusv3 4) [1,2,3]
-- 

mapv2 :: (a->b) -> [a] -> [b]
mapv2 f xs = [f x| x <- xs]

mapv3 :: (a->b) -> [a] -> [b]
mapv3 f [] = []
mapv3 f (x:xs) = f x : map f xs

filterv2 :: (a -> Bool) -> [a] -> [a]
filterv2 f xs = [x | x <- xs, f x == True]