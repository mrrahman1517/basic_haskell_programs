qs [] = []
qs (x:xs) = qs ys ++ [x] ++ qs zs
            where
                ys = [a | a <- xs, a <= x]
                zs = [a | a <- xs, a > x]