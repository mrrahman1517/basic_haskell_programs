import Control.Parallel (par, pseq)
import Control.DeepSeq (force, NFData)

addTwo :: [Int] -> [Int]
addTwo [] = []
addTwo (x:xs) = (x+2): addTwo xs 


myqsort :: (Ord a) => [a]->[a]
myqsort [] = []
myqsort (x:xs) = leftlist ++ [x] ++ rightlist 
         where leftlist = myqsort [l | l <- xs, l < x]
               rightlist = myqsort [r | r <- xs, r >= x] 

-- parallelize sort

-- usage
--ghci -package parallel -package deepseq all_about_lists.hs
--ghci> verifysort (pqsort [1,20,11,34,1,10,45545,-2])

pqsort :: (Ord a, NFData a) => [a] -> [a]
pqsort [] = []
pqsort (x:xs) = force greater `par` 
    (force lesser `pseq` (lesser ++ [x] ++ greater))
      where lesser = pqsort [y | y <- xs, y < x]
            greater = pqsort [y | y <- xs, y >= x]

verifysort :: (Ord a) => [a] -> Bool
verifysort [] = True
verifysort (x:xs) = length [(x1,x2)| (x1,x2) <- (zip (x:xs) xs), x1 > x2] == 0 

-- more efficient

-- simple recursive (no intermediate structures)
verifysortv2 :: Ord a => [a] -> Bool
verifysortv2 []         = True
verifysortv2 [_]        = True
verifysortv2 (x:y:xs)   = x <= y && verifysortv2 (y:xs)
