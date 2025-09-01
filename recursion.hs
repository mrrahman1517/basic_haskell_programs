-- Collection of recursive and list processing functions

-- | Fibonacci number calculator
-- Returns the nth number in the Fibonacci sequence
-- Example: fib 5 = 5 (because 0,1,1,2,3,5,...)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | Factorial calculator using list product
-- Calculates n! using the built-in product function
-- Example: fac 4 = 24 (because 4 * 3 * 2 * 1)
fac :: Int -> Int 
fac n = product [1..n]

-- | Recursive factorial calculator
-- Calculates n! using explicit recursion
-- Example: facr 4 = 24
facr :: Int -> Int 
facr 0 = 1
facr n = n * facr (n-1)

-- | Product of a list of numbers
-- Recursively multiplies all elements in a list
-- Example: myproduct [2,3,4] = 24
myproduct :: Num a => [a] -> a
myproduct [] = 1
myproduct (n:ns) = n * myproduct ns

-- | Length of a list
-- Counts the number of elements in a list
-- Example: mylength [1,2,3] = 3
mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = 1 + mylength xs

-- | Reverse a list
-- Returns a new list with elements in reverse order
-- Example: myreverse [1,2,3] = [3,2,1]
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = reverse xs ++ [x]

-- | Zip two lists together
-- Combines corresponding elements into pairs
-- Example: myzip [1,2] ['a','b'] = [(1,'a'),(2,'b')]
myzip :: [a] ->[b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : zip xs ys

-- | Drop first n elements
-- Removes the first n elements from a list
-- Example: mydrop 2 [1,2,3,4] = [3,4]
mydrop :: Int -> [Int] -> [Int]
mydrop 0 xs = xs
mydrop _ [] = []
mydrop n (_:xs) = mydrop (n-1) xs

-- | Append two lists
-- Combines two lists by adding the second list to the end of the first
-- Example: myappend [1,2] [3,4] = [1,2,3,4]
myappend :: [a] -> [a] -> [a]
myappend [] ys = ys
myappend (x:xs) ys = x : myappend xs ys

-- | Quicksort implementation
-- Sorts a list using the quicksort algorithm
-- Example: myqs [3,1,4,1,5] = [1,1,3,4,5]
myqs :: Ord a => [a] -> [a]
myqs [] = []
myqs (x:xs) = myqs ls ++ [x] ++ myqs rs 
              where ls = [l | l <- xs, l <= x]
                    rs = [r | r <- xs, r > x]

-- | Boolean AND operation
-- Returns True only if both inputs are True
-- Example: myand True False = False
myand :: Bool -> Bool -> Bool 
myand False _ = False 
myand True b = b

-- | Check if all elements are True
-- Returns True only if all elements in the list are True
-- Example: myalland [True,True,True] = True
myalland :: [Bool] -> Bool 
myalland [] = True 
myalland (b:bs) = myand b (myalland bs) 

-- | Concatenate list of lists
-- Combines multiple lists into a single list
-- Example: myconcat [[1,2],[3,4]] = [1,2,3,4]
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (xs:xss) = xs ++ (myconcat xss)

-- produce a list with n identical elements
myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate 1 x = [x]
myreplicate n x = x : myreplicate (n-1) x

-- select the nth element of a list
-- assume 0 <= n <= length of list
mid :: [a] -> Int -> a
mid (x:xs) 0 = x
mid (x:xs) n = mid xs (n-1) 

-- decide if a value is an element of a list
melem :: Eq a => a -> [a] -> Bool
melem v [] = False
melem v (x:xs) | v == x = True
               | otherwise = melem v xs 

-- merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys 
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- mergesort
--mergesort :: Ord a => [a] -> [a]
--mergsort [] = []
--mergsort 


-- first element of a list
firstv2 :: Num a => [a] -> a 
firstv2 [] = 0
firstv2 (x:xs) = x
-- return second element of a list
secondv2 :: Num a => [a] -> a 
secondv2 [] = 0
secondv2 (x:xs) = firstv2 xs

secondv3 :: [Int] -> Int 
secondv3 [] = 0 -- empty list has no second element
secondv3 (x:[]) = 0 -- list of length 1 has no second element
secondv3 (x:y:xs) = y -- otherwise ...



plusv2 :: Int -> Int -> Int 
plusv2 x y = x + y 

sucv2 :: Int -> Int 
sucv2 = plusv2 1

headv2 :: [Int] -> Int 
headv2 [] = 0
headv2 (x:xs) = x

tailv2 :: [a] -> [a]
tailv2 [] = []
tailv2 (x:xs) = xs 

secondv4 :: [Int] -> Int 
secondv4 [] = 0
secondv4 (x:[]) = 0
secondv4 xs = headv2 (tailv2 xs) 