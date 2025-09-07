
-- fib
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- traingle number
trianglev2 :: Int -> Int 
trianglev2 0 = 0
--trianglev2 1 = 1
trianglev2 n = trianglev2 (n-1) + n 