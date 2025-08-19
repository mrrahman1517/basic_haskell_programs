in_range :: Int -> Int -> Int -> Bool   
in_range min max x = 
    let in_lower_bound = min <= x
        in_upper_bound = max >= x
    in 
    in_lower_bound && in_upper_bound