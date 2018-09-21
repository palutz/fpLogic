-- Chapter 02 - Mathematical Objects

module Chapter02

where 

-- logical connectives:
-- not, if, and or, if and only if (iff), for all, for some 

-- example of declaration (already declared in prelude
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

-- Disjunction
-- or :: Bool -> Bool -> Bool
(||!) :: Bool -> Bool -> Bool
True  ||! x = True
False ||! x = x

-- Implication 
-- if P then Q or Q if P; implication of P and Q
-- implication is true if 
--    both P (antecedent) and Q (consequent) are false
--    P (antecedent) is false and Q (consequent) is true
--    both antecedent and consequent are true
-- if :: Bool -> Bool -> Bool
(==>) :: Bool -> Bool -> Bool
-- x ==> y = not x ||! y
-- or other implementation
True  ==> x = x
False ==> x = True
