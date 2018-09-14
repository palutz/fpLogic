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

-- 1.10) removeFst, removes the 1st occurence of an int from a list. if no occurence list is unchanged
removeFst :: Int -> [Int] -> [Int]
removeFst y [] = []
removeFst y [x] = if x == y then []
                  else [x]
removeFst y (x:xs) =  if x == y then xs
                      else x : removeFst y xs  -- append the 1st element to the result of the recursive call

-- 1.11) define a function that sorts a list of int in order of increasing size:
-- - if the list is empty ,the list is already sorted
-- - if the list is not empty, put the minimum in front of the new list resulting on the prev removing the minimum
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

-- 1.12) function that calculare the average of a list
average :: [Int] -> Rational
average [] = 0.0
average xs = toRational (sum xs) / toRational (length xs)

-- homemade version of the sum 
sum' :: [Int] -> Int 
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

-- homemade version of length
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs


-- 1.13) counting the occurence of a char in a string 
count' :: Char -> String -> Int 
count' c [] = 0
count' c (x:xs) = if x == c then 1 + (count' c xs)
                            else count' c xs

-- 1.14) write a function blowup that transform a string a1a2a3 .. in a1a2a2a3a3a3,,,,
-- eg. bang -> baannngggg
repChar :: Char -> Int -> String
repChar c n = if n > 0 then [c] ++ repChar c (n - 1)
                else []

innerBlow :: String -> Int -> String -> String
innerBlow [] _ acc = acc
innerBlow (x:xs) n acc = innerBlow xs (n + 1) (acc ++ (repChar x (n + 1)))

blowUp :: String -> String
blowUp s = innerBlow s 0 ""

-- 1.15) Sort a list of string in alphabetical order
-- my min for string list
minStrs :: [String] -> String
minStrs [] = []
minStrs [x] = x
minStrs (x:xs) = min x (minStrs xs)

-- remove first for string
removeFstS :: String -> [String] -> [String]
removeFstS _ [] = []
removeFstS c [x] = if x == c then []
                   else [x]
removeFstS c (x:xs) = if c == x then xs
                      else x : (removeFstS c xs)

-- the actual sorting 
srtStrings :: [String] -> [String]
srtStrings [] = []
srtStrings (xs) = s1 :srtStrings (srtStrings (removeFstS s1 xs)) where s1 = minStrs xs

-- 1.16) check if a string (str1) is a prefix of the other (str2)
-- if 1st empty then prefix is true whatever will be the value of 2nd
-- if 2nd is empty prefix will always be false
-- otherwise we check the value of the first letter for both strings
prefix :: String -> String -> Bool
prefix [] _ = True
prefix xs [] = False
prefix (x:xs) (y:ys) = (x==y) && (prefix xs ys)


-- 1.17) find if x is a substring of y
-- if xs is a prefix of ys, then xs is a prefix of ys
-- if ys == y:ys' and xs is a prefix of ys' then xs is a substring of ys
-- otherwise is false
substring :: String -> String -> Bool
substring [] _ = True
substring xs [] = False
substring xs (y:ys) = if prefix xs (y:ys) then True
                          else substring xs ys

-- Haskell type game...
a :: [String]
a = ["st1", "st2", "st3", "...", "stn"]
b :: (Bool, String)
b = (True, "this is True")
c :: [(Bool, String)]
c = [(True, "This is"), (False, "This is not"), (True, "True")]
d :: ([Bool], String)
d = ([True, True, False], "list of")
e :: Bool -> Bool
e b = b
f' :: Int -> String -> Bool
f' a1 b1 = (length b1) > a1
f2 = flip (f')  -- f2 :: String -> Int -> Bool

-- Prime factiorization algo
factors :: Integer -> [Integer]
factors n | n < 0 = error "negative number"
          | n <= 1 = []
          | otherwise = p : factors (div n p) where p = ld n

-- map 
-- map :: (a -> b) -> [a] -> [b]
m1 = map (2 ^) [1..10]   -- [2,4,8,16,32,64,128,256,512,1024]
m2 = map (^ 2) [1..10]   -- [1,4,9,16,25,36,49,64,81,100]

-- create my own map
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = (f x) : (mymap f xs)

-- 1.20 map a list of string to a list of string length
strLengths :: [String] -> [Int]
strLengths xs = map (length) xs

-- 1.21 - SumLengths - take list of lists and return the sum of their lengths
sumLengths :: [[Char]] -> Int
sumLengths l = sum (strLengths l)

-- myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) | f x = x : myFilter f xs
                  | otherwise = myFilter f xs
