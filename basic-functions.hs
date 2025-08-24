-- | Returns the absolute value of an integer
myabs :: Int -> Int
myabs n = if n >= 0 then n else - n

-- | Returns the sign of an integer: -1, 0, or 1
mysignum :: Int -> Int
mysignum n = if n < 0 then -1 else 
              if n == 0 then 0 else 1

-- | Absolute value using guards
gabs :: Int -> Int
gabs n | n >= 0 = n 
       | otherwise = -n

-- | Signum function using guards
gsignum n | n < 0     = -1 
          | n == 0    = 0 
          | otherwise = 1

-- | Negates a boolean value
mynot :: Bool -> Bool 
mynot False = True
mynot True = False

-- | Logical 'and' implemented with all cases
myand1 :: Bool -> Bool -> Bool
myand1 True True = True
myand1 True False = False
myand1 False True = False
myand1 False False = False

-- | Logical 'and' (True only if both True)
myand2 :: Bool -> Bool -> Bool 
myand2 True True = True
myand2 _    _    = False

-- | Logical 'and' using pattern matching
myand3 :: Bool -> Bool -> Bool 
myand3 False _ = False
myand3 True  b = b

-- | Returns the first element of a list (like car in Lisp)
myhead :: [a] -> a 
myhead (x:_) = x

-- | Returns the tail of a list (like cdr in Lisp)
mytail :: [a] -> [a]
mytail (_:xs) = xs

-- | Generates a list of odd numbers less than 2n, using a local function
odds n = map f [0..n-1]
         where 
              f x = x * 2 +1

-- | Generates a list of odd numbers using a lambda expression
lodds n = map (\x -> x * 2 + 1) [0 .. n-1]

-- | Returns the tail of a list, returns [] for empty list
safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

-- | Safe tail, returns [] for empty list, otherwise uses 'tail'
mysafetail :: [a] ->[a]
mysafetail [] = []
mysafetail xs = tail xs

-- | Safe tail using 'if' and 'null'
mysafetail1 :: [a] ->[a]
mysafetail1 xs = if null xs then
                    []
                 else 
                    tail xs

-- | Safe tail using guards
safetail2 :: [a] -> [a]
safetail2 xs | null xs = xs
             | otherwise = tail xs

-- | Logical 'or' implemented with pattern matching
myor :: Bool -> Bool -> Bool
myor True _ = True
myor _ True = True 
myor _ _ = False

-- | Logical 'or' (False only if both False)
myor1 :: Bool -> Bool -> Bool
myor1 False False = False
myor1 _ _ = True

-- | Logical 'or' using pattern matching
myor2 :: Bool -> Bool -> Bool
myor2 False b = b
myor2 True _ = True

-- | Concatenates a list of lists into a single list
myconcat :: [[a]] -> [a]
myconcat xss = [x| xs <- xss, x <- xs]

factors :: Int -> [Int]
factors n = [d | d <- [1..n], mod n d == 0]

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

-- no lazy evaluation 
prime :: Int -> Bool
prime n = len (factors n) == 2

-- (0.00 secs, 65,288 bytes)
--ghci> prime 10000000
--False
--(1.72 secs, 2,000,075,560 bytes)
--ghci> prime2 10000000

--prime 100000000
--False
--(17.09 secs, 20,000,078,424 bytes)
--ghci> prime2 100000000
--False
--(0.00 secs, 65,288 bytes)
-- for timing use: +set +s

-- lazy evaluation, as long as factors call finds a factor other
-- other than 1 or n, the equality will fail and return false
prime2 :: Int -> Bool
prime2 n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime2 x]
