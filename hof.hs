-- | Apply a function twice to a value
-- Example: twice (+1) 3 = 5
twice :: (a->a)->a->a
twice f x = f (f x)

-- | Square a number
-- Example: sqr 4 = 16
sqr :: Int -> Int 
sqr x = x * x

-- | Add two integers
-- Example: plusv3 2 3 = 5
plusv3 :: Int -> Int -> Int 
plusv3 x y = x + y

-- | Increment an integer by 1
-- Example: incv3 5 = 6
incv3 :: Int -> Int 
incv3 = plusv3 1

-- twice incv3 31
-- map (plusv3 4) [1,2,3]
-- 

mapv2 :: (a->b) -> [a] -> [b]
mapv2 f xs = [f x| x <- xs]

-- | Map a function over a list (list comprehension)
-- Example: mapv2 (+1) [1,2,3] = [2,3,4]
mapv3 :: (a->b) -> [a] -> [b]
mapv3 f [] = []
mapv3 f (x:xs) = f x : map f xs

-- | Filter a list using a predicate (list comprehension)
-- Example: filterv2 even [1,2,3,4] = [2,4]
filterv2 :: (a -> Bool) -> [a] -> [a]
filterv2 f xs = [x | x <- xs, f x == True]

-- | Filter a list using a predicate (recursion)
-- Example: filterv3 even [1,2,3,4] = [2,4]
filterv3 :: (a -> Bool) -> [a] -> [a]
filterv3 p [] = []
filterv3 p (x:xs)
          | p x = x : filterv3 p xs 
          | otherwise = filterv3 p xs 


sumv5 :: [Int] -> Int
sumv5 = foldr (+) 0

productv5 :: [Int] -> Int 
productv5 = foldr (*) 1

-- | Sum a list of integers using foldr
-- Example: sumv5 [1,2,3] = 6
orv5 :: [Bool] -> Bool 
orv5 = foldr (||) False

-- | Product of a list of integers using foldr
-- Example: productv5 [2,3,4] = 24
andv5 :: [Bool] -> Bool 
andv5 = foldr (&&) True 

-- | Logical OR of a list of booleans using foldr
-- Example: orv5 [False, True, False] = True
lengthv5 :: [Int] -> Int 
lengthv5 = foldr (\ _ n -> n + 1) 0

-- | Logical AND of a list of booleans using foldr
-- Example: andv5 [True, True, False] = False
fldreverse :: [Int] -> [Int]
fldreverse = foldr (\ x xs -> xs ++ [x]) []

-- | Length of a list using foldr
-- Example: lengthv5 [1,2,3] = 3
foldappend :: [Int] -> [Int] -> [Int]
foldappend xs ys = foldr (:) ys xs

-- | Reverse a list using foldr
-- Example: fldreverse [1,2,3] = [3,2,1]
oddv7 :: Int -> Bool 
oddv7 = not . even

-- | Append two lists using foldr
-- Example: foldappend [1,2] [3,4] = [1,2,3,4]
compose :: (b->c) -> (a->b) -> (a->c)
compose f g = \x -> f (g x)

-- | Check if an integer is odd (using function composition)
-- Example: oddv7 3 = True
oddv8 :: Int -> Bool 
oddv8 = compose not even 

-- | Compose two functions
-- Example: compose (*2) (+1) $ 3 = 8
allv1 :: (a -> Bool) -> [a] -> Bool 
allv1 p xs = and [p x | x <- xs]

-- | Check if an integer is odd (using compose)
-- Example: oddv8 3 = True
--ghci> anyv1 (== 1) [2,4]
--False
--ghci> anyv1 (== 1) [2,4,1]
-- | Check if all elements satisfy a predicate
-- Example: allv1 even [2,4,6] = True
--True
anyv1 :: (a -> Bool) -> [a] -> Bool 
anyv1 p xs = or [p x | x <- xs] 
-- | Check if any element satisfies a predicate
-- Example: anyv1 (== 1) [2,4,1] = True

-- takewhilev2 (/= ' ') "abc def" = "abc"
-- | Take elements from a list while a predicate is true
-- Example: takewhilev2 (/= ' ') "abc def" = "abc"
takewhilev2 :: (a -> Bool) -> [a] -> [a]
takewhilev2 p [] = []
takewhilev2 p (x:xs)
                    | p x = x : takewhilev2 p xs
                    | otherwise = []


dropWhilev2 :: (a->Bool)->[a]->[a]
dropWhilev2 p [] = []
dropWhilev2 p (x:xs)
      | p x = dropWhilev2 p xs 
      | otherwise = x:xs


-- tokinzer "abc def" = ["abc","def"]
-- simple string tokenizer
--tokenize :: [a] -> [[a]]
 --tokenize [] = []
 --tokenize xs = [x| x <- takewhilev2 xs, ] 



fibv8 :: Int -> Int
fibv8 0 = 1
fibv8 1 = 1
fibv8 n = fibv8 (n-1) + fibv8 (n-2)

mydrop :: Int -> [a] ->[a]
--drop 0 [] = []
mydrop 0 xs = xs 
mydrop _ [] = []
mydrop n (_:xs) = mydrop (n-1) xs 

-- define a function that removes the last element from a non-empty list
-- haskell type system cannot express non-empty list
-- for that we need to use dependently typed languages such as agda or idris
myinit :: [a] ->[a]
--init [] = []
myinit (x:xs) | null xs = []
            | otherwise = x : myinit xs 

myinitv2 :: [a] -> [a]
myinitv2 [x] = []
myinitv2 (x:xs) = x : myinitv2 xs