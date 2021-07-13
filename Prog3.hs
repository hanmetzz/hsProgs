-- Hannah Metzler Prog3.hs 10/15/19

module Prog3 where

import Data.Char
--productLastPart returns the products of the last n numbers in the list using
--only library functions
productLastPart :: Int -> [Int] -> Int
productLastPart x ys = product (drop (length ys - x) ys)

--init' has identical behavior to init function only using std haskell fxns
init' :: [Int] -> [Int]
init' [] = []
init' x = take (length x - 1) x

--init'' identical behavior to init without std haskell fxns
init'' :: [Int] -> [Int]
init'' [y] = []
init'' (x:xs) = x:init'' xs

--elemAt returns the ith item of the list, where 1st item is index 1
elemAt :: Int -> [Int] -> Int
elemAt y (x:xs)
  | y == 1    = x
  | otherwise = elemAt(y-1) xs
 
--numTimes returns the number of times an element occurs in a list
numTimes :: Int -> [Int] -> Int
numTimes x [] = 0
numTimes x (y:ys)
  | x /= y    = numTimes x ys
  | otherwise = 1 + numTimes x ys

--lowerFirstLetter lowercases the first letter
lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter (x:xs) = (toLower x):xs

--nestedParens returns true if there are nested parens
nestedParens :: String -> Bool
nestedParens [] = False
nestedParens "()" = True
nestedParens x 
  | head x == '(' && last x == ')' = nestedParens (init(drop 1 x)) 
  | otherwise = False

--triads generates a list of int triples
triads :: Int -> [(Int,Int,Int)]
triads n = [(x,y,z) | x<-[1..100], y <- [1..100], z <- [1..100], ((x^2)+(y^2))== (z^2) && x < n && y < n && z < n]

--iSort' uses insertion to sort a list of triples by the 2nd elmt increasing
ins :: (Float,Int,String) -> [(Float,Int,String)] -> [(Float,Int,String)]
ins (x,y,z) [] = [(x,y,z)]
ins (x,y,z) ((a,b,c):ys)
  | y <= b   = ((x,y,z):(a,b,c):ys)
  | otherwise   = (a,b,c): ins (x,y,z) ys

iSort' :: [(Float,Int,String)] -> [(Float,Int,String)]
iSort' (x:xs) = ins x (iSort' xs)
iSort' [] = []

--merge takes two sorted lists and merges them into a single sorted list
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] xs = xs
merge ys [] = ys
merge (y:ys) (x:xs)
  | y > x   = y: merge (x:xs) ys
  | otherwise = x: merge (y:ys) xs
