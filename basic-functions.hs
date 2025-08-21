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

myhead :: [a] -> a 
myhead (x:_) = x

mytail :: [a] -> [a]
mytail (_:xs) = xs