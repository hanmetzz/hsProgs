{- Name: Hannah Metzler
Date: 3/7/19
Prog3.hs -}

module Prog3 where
--sumLastPart
sumLastPart :: Int -> [Int] -> Int
sumLastPart a b
    | a > length b = sumLastPart a (tail b)
    | otherwise    = sum b

--init' has identical behavior to the init function using only list standard fu$
init' :: [Int] -> [Int]
init' [x]   = []
init' (x:xs)= x : init' xs

--init'' has identical behavior to init without standard Haskell functions
init'' :: [Int] -> [Int]
init'' [x] = []
init'' (x:xs) = init'' xs

--elemAt returns the ith item of the list where first item is index 1
elemAt :: Int -> [Int] -> Int
elemAt 1 (y:ys) = y
elemAt n (z:zs) = elemAt (n-1) zs

--numTimes returns the # of times that an element occurs in a list using recurs$
numTimes :: Int -> [Int] -> Int
numTimes _ [x] = 0
numTimes n (x:xs) = [x |x <- xs, x == n]

--lowerFirstLetter makes the first letter of a string lowercase
lowerFirstLetter :: String -> String
lowerFirstLetter = [toLower x | x <- xs ,x >= 'A' && x <= 'Z']

--and' uses recursion to return the conjunction of a list of boolean vals
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

--or' uses recursion to return the disjunction of a list of boolean vals
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

--iSort' uses insertion to sort a list of triples
--iSort' :: [(Float, Int, String)] -> [(Float, Int, String)]

--merge takes two sorted lists and merges them into a single sorted list
merge :: [Int] -> [Int] -> [Int]
merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs) = a b merge as bs
