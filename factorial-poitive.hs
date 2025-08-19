-- Factorial that does not handle negative inputs

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))
