-- | Merge two sorted lists
-- Takes two sorted lists and combines them into a single sorted list
-- Example: merge [1,3,5] [2,4,6] = [1,2,3,4,5,6]
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys            -- if first list is empty, return second list
merge xs [] = xs            -- if second list is empty, return first list
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)    -- take x if it's smaller
    | otherwise = y : merge (x:xs) ys     -- take y if it's smaller

