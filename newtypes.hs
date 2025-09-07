type Stringv2 = [Char]

lengthv6 :: Stringv2 -> Int 
lengthv6 [] = 0
lengthv6 (x:xs) = 1 + lengthv6 xs 