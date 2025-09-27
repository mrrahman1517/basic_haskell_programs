addTwo :: [Int] -> [Int]
addTwo [] = []
addTwo (x:xs) = (x+2): addTwo xs 


myqsort :: (Ord a) => [a]->[a]
myqsort [] = []
myqsort (x:xs) = leftlist ++ [x] ++ rightlist 
         where leftlist = myqsort [l | l <- xs, l < x]
               rightlist = myqsort [r | r <- xs, r >= x] 

verifysort :: (Ord a) => [a] -> Bool
verifysort [] = True
verifysort (x:xs) = length [(x1,x2)| (x1,x2) <- (zip (x:xs) xs), x1 > x2] == 0 