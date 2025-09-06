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

filterv3 :: (a -> Bool) -> [a] -> [a]
filterv3 p [] = []
filterv3 p (x:xs)
          | p x = x : filterv3 p xs 
          | otherwise = filterv3 p xs 


sumv5 :: [Int] -> Int
sumv5 = foldr (+) 0

productv5 :: [Int] -> Int 
productv5 = foldr (*) 1

orv5 :: [Bool] -> Bool 
orv5 = foldr (||) False

andv5 :: [Bool] -> Bool 
andv5 = foldr (&&) True 

lengthv5 :: [Int] -> Int 
lengthv5 = foldr (\ _ n -> n + 1) 0

fldreverse :: [Int] -> [Int]
fldreverse = foldr (\ x xs -> xs ++ [x]) []

foldappend :: [Int] -> [Int] -> [Int]
foldappend xs ys = foldr (:) ys xs

oddv7 :: Int -> Bool 
oddv7 = not . even

compose :: (b->c) -> (a->b) -> (a->c)
compose f g = \x -> f (g x)

oddv8 :: Int -> Bool 
oddv8 = compose not even 

allv1 :: (a -> Bool) -> [a] -> Bool 
allv1 p xs = and [p x | x <- xs]

--ghci> anyv1 (== 1) [2,4]
--False
--ghci> anyv1 (== 1) [2,4,1]
--True
anyv1 :: (a -> Bool) -> [a] -> Bool 
anyv1 p xs = or [p x | x <- xs] 

-- takewhilev2 (/= ' ') "abc def" = "abc"
takewhilev2 :: (a -> Bool) -> [a] -> [a]
takewhilev2 p [] = []
takewhilev2 p (x:xs)
                    | p x = x : takewhilev2 p xs
                    | otherwise = []