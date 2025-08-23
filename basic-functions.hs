myabs :: Int -> Int
myabs n = if n >= 0 then n else - n

mysignum :: Int -> Int
mysignum n = if n < 0 then -1 else 
              if n == 0 then 0 else 1

gabs :: Int -> Int
gabs n | n >= 0 = n 
       | otherwise = -n

gsignum n | n < 0     = -1 
          | n == 0    = 0 
          | otherwise = 1

mynot :: Bool -> Bool 
mynot False = True
mynot True = False

myand1 :: Bool -> Bool -> Bool
myand1 True True = True
myand1 True False = False
myand1 False True = False
myand1 False False = False

myand2 :: Bool -> Bool -> Bool 
myand2 True True = True
myand2 _    _    = False

myand3 :: Bool -> Bool -> Bool 
myand3 False _ = False
myand3 True  b = b

-- def car
myhead :: [a] -> a 
myhead (x:_) = x

-- def cdr
mytail :: [a] -> [a]
mytail (_:xs) = xs

odds n = map f [0..n-1]
         where 
              f x = x * 2 +1

lodds n = map (\x -> x * 2 + 1) [0 .. n-1]

safetail :: [a] -> [a]
safetail [] = []
safetail (_:xs) = xs

mysafetail :: [a] ->[a]
mysafetail [] = []
mysafetail xs = tail xs

mysafetail1 :: [a] ->[a]
mysafetail1 xs = if null xs then
                    []
                 else 
                    tail xs

safetail2 :: [a] -> [a]
safetail2 xs | null xs = xs
             | otherwise = tail xs

myor :: Bool -> Bool -> Bool
myor True _ = True
myor _ True = True 
myor _ _ = False

