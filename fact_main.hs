-- factorial.hs

factorial :: Integer -> Integer
factorial 0 = 1
factorial n 
    | n > 1 = n * factorial (n - 1)
    | n < 0 = factorial(-n)

main :: IO ()
main = do
    putStrLn "Enter a number:"
    input <- getLine
    let n = read input :: Integer
    putStrLn ("Factorial of " ++ show n ++ " is " ++ show (factorial n))
