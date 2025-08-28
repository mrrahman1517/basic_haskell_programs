-- recursive functions

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- factorial
fac :: Int -> Int 
fac n = product [1..n]

facr :: Int -> Int 
facr 0 = 1
facr n = n * facr (n-1)

myproduct :: Num a => [a] -> a
myproduct [] = 1
myproduct (n:ns) = n * myproduct ns

mylength :: [a] -> Int
mylength [] = 0
mylength (_:xs) = 1 + mylength xs