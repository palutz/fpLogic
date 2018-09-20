-- Chapter 02 - Mathematical Objects

module Chapter02

where 

-- logical connectives:
-- not, if, and or, if and only if (iff), for all, for some 

-- data Bool = False | True


-- Negation
-- not :: Bool -> Bool
not1 :: Bool -> Bool
not1 True = False
not1 False = True

-- Conjunction 
-- and :: Bool -> Bool -> Bool 
(&&!) :: Bool -> Bool -> Bool 
False &&! x = False
True  &&! x = x
