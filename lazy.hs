ones = 1:ones

-- innermost
-- head ones
-- head (1:ones)
-- head (1:(1:ones))
-- non terminating

--lazy
-- head ones
-- head (1:ones)
-- 1

-- terminate in 2 steps

-- take 5 ones
-- only evaluate as much as needed from infinite list
-- modular programming

-- generate all primes
-- 1. write down inf seq of all numbers 2, ,3, 4, ...
-- 2. mark first num p as prime
-- 3. delete all multiples of p
-- 4. return to step 2

primes = sieve [2..]

sieve (p:xs) = p: sieve [x | x <- xs, mod x p /= 0]

-- take 10 primes
-- only takes first 10 primes from infinite list

plen :: [Int] -> Int 
plen [] = 0
plen (x:xs) = 1 + plen xs

twin (x,y) = y ==  x + 2

twins = filter twin (zip primes (tail primes))

-- take 1000 twins
-- generate the first 1000 twin prime pairs