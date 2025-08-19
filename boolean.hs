-- basic boolean logic

-- xor

cxor:: Bool -> Bool -> Bool
cxor True False = True
cxor False True = True
cxor b1 b2 = False

-- and

cand :: Bool -> Bool -> Bool
cand True b = b
cand False _ = False

-- or

cor :: Bool -> Bool -> Bool
cor True _ = True
cor _ True = True
cor _ _ = False
