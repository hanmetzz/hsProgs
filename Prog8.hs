{- ###############
  Hannah Metzler
   Prog8.hs
    12/7/19
  ############## -}

module Prog8 where

--sumSqNeg computes the sum of squares of negatives
sumSqNeg :: [Int] -> Int
sumSqNeg [] = 0
sumSqNeg xs = sum (map (^2) (filter (<0) xs))

--containing returns True if each element in the first list is also in the 2nd
containing :: Eq a => [a] -> [a] -> Bool
containing [] [] = True
containing _ [] = False
containing [] _ = True
containing xs ys = and[(( x `elem` ys) == True)|x <- xs]

--total applies the function to every element in the list and sums 
total :: (Int -> Int) -> [Int] -> Int
total x xs = sum (map(x) (xs))

--containing' returns whether each elemen in the first list is also in the 2nd
containing' :: Eq a => [a] -> [a] -> Bool
containing' [] [] = True
containing' _ [] = False
containing' [] _ = True
containing' a b = foldr (&&) True (map(`elem` b) a)

--lengths returns a list of lengths of the given strings
lengths :: [String] -> [Int]
lengths xs = map (length) xs

--product' returns the product of a nonempty list of nums
product' :: Num a => [a] -> a
product' x = foldr (*) 1 x

--max' returns the largest element of a nonempty list
max' :: Ord a => [a] -> a
max' (x:[]) = x
max' (x:y:ys)
  |x >= y = max' (x:(filter(/=y) ys))
  |otherwise = max' (y:(filter(/=x) ys))

--append' appends two lists together
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (\x y -> x:y) xs ys

--filterFirst removes the first element from the list that doesnt satisfy a given predicate function
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst x xs = foldr (:) [] (filter' x xs)
  where
    filter' _ [] = []
    filter' y (x:xs)
      |y x = x:filter' y xs
      |otherwise = xs

--filterLast removes the last element from the list
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast _ [] = []
filterLast x xs = reverse(filterFirst x(reverse xs))
