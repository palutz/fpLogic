-- Haskell type game...

module TypeGame01 where 

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

