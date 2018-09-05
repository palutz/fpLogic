module Steo_GS

where 

-- Prime number
--
-- Prime number test 
-- Proposition to implemtent the prime number test:
-- 1) if n > 1 then LD(n) is a prime number
-- 2) if n > 1 and n is not prime, then (LD(n))^2 <= n

-- divdes denominator numbertodivide 
-- simple type definition
-- divides :: Integer -> Integer -> Bool
-- more correct:
divides :: Integral a => a -> a -> Bool 
divides d n = rem n d == 0

-- least divisor function
ld :: Integer -> Integer
ld n = ldf 2 n

-- (inner) least divisor function implementation
ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n

-- prime0 - first implementation of the prime number function
prime0 :: Integer -> Bool
prime0 n  | n < 1     = error "not a positive integer"
          | n == 1    = False
          | otherwise = ld n == n 

-- using guarded equations (not abbreviated) 
prime0b :: Integer -> Bool
prime0b n | n < 1     = error "not a positive integer"
prime0b n | n == 1    = False
prime0b n             = ld n == n 

-- get the minimun of a list
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)
