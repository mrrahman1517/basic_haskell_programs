double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = div (sum ns) (length ns)

avg ns = sum ns `div` length ns

n = a `div` (length xs)
    where
       a  = 10
       xs = [1,2,3,4,5]

lastElement [] = 0
lastElement xs = (reverse xs) !! 0

mylast [] = 0
mylast xs = xs !! (length xs - 1)

mylast2 [] = 0
mylast2 xs = head (reverse xs)

myinit xs = take (length xs - 1) xs

myinith xs = reverse (tail (reverse xs)) 

not :: Bool -> Bool
not True = False
not False = True

myeven :: Int -> Bool
myeven x = x `mod` 2 == 0
    