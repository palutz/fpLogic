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
False &&! _ = False
True  &&! x = x

-- Disjunction
-- or :: Bool -> Bool -> Bool
(||!) :: Bool -> Bool -> Bool
True  ||! _ = True
False ||! x = x

-- Implication ( => )
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

-- CONVERSE AND CONTRAPOSITIVE 
-- Converse P => Q is Q => P (if P => Q, Q => could not be true)
-- Contrapositive P => Q is !Q => !P  (!Q => !P is true iff P => Q)
-- NECESSARY AND SUFFICIENT CONDITIONS
-- if P => Q holds then
-- P is a sufficient condition for Q and Q is a necessary condition for P 
-- We can express P => Q like
-- if P then Q 
-- Q if P
-- P only if Q 
-- Q whatever P 
-- P is sufficient for Q 
-- Q is necessary for P 
