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

-- homemade min function
min' :: Int -> Int -> Int
min' x y  | x <= y = x
          | otherwise = y

-- homemade max function
max' :: Int -> Int -> Int
max' x y  | x >= y = x
          | otherwise = y

-- 1.9) Define a function that returns the max of a list
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max' x (maxInt xs)

-- 1.10 - removeFst, removes the 1st occurence of an int from a list. if no occurence list is unchanged
removeFst :: [Int] -> Int -> [Int]
removeFst [] y = []
removeFst [x] y = if x == y then []
                  else [x]
removeFst (x:xs) y =  rmvFstInner (x:xs) y []

-- recursive (iunner implementation of removeFst
rmvFstInner :: [Int] -> Int -> [Int] -> [Int]
rmvFstInner [] y r = r
rmvFstInner [x] y r = if x == y then r
                      else (r ++ [x])
rmvFstInner (x:xs) y r = if x == y then (r ++ xs)
                         else rmvFstInner xs y (r ++ [x])
