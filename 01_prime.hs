module GS

where 

-- divdes denominator numbertodivide 
divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

-- least divisor function
ld :: Integer -> Integer
ld n = ldf 2 n

-- (inner) least divisor function implementation
ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 >= n = n
        | otherwise = ldf (k+1) n

-- prime0 - first implementation of the prime number function
prime0 :: Integer -> Bool
prime0 n  | n < 1     = error "not a positive integer"
          | n == 1    = False
          | otherwise = ld n == n 
