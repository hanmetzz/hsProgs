{- #############
   Hannah Metzler
   CSC345 Prog5.hs
   due: 10/29/19
   ############# -}

module Prog5 where

--reverse' reverses a list using case expressions and no built in haskell functions
reverse' :: [a] -> [a]
reverse' x = case x of
  [] -> []
  (x:xs) -> (reverse' xs) ++ [x]

--isPalindrome returns if some string can be read forward and backward
isPalindrome :: String -> Bool
isPalindrome xs = xs == (reverse' xs)

--safeFindAfter returns the remainder of a list after a given string is found from given list
safeFindAfter :: String -> [String] -> Maybe [String]
safeFindAfter _ [] = Nothing
safeFindAfter str (x:xs) = case x == str of
  True -> Just xs
  False -> safeFindAfter str xs

data Set = Set [Char]
  | EmptySet
    deriving Show

--member checks if a given item is present in a set
member :: Char -> Set -> Bool
member _ (Set [])   = False
member _ (EmptySet) = False
member n (Set (x:xs))
  | n == x     = True
  | n /= x     = member n (Set(xs))
  | otherwise  = False

--size returns the number of elements in a given set
size :: Set -> Int
size EmptySet = 0
size (Set x)  = length x

--add adds a given item into a set
add :: Char -> Set -> Set
add a (EmptySet) = (Set(a:[]))
add a (Set(x))
  | (member a (Set (x))) == True  = Set (x)
  | otherwise                     = (Set (a:x))

--equal returns true if two given sets are equal
equal :: Set -> Set -> Bool
equal (Set(x:[])) (Set(y))  = member x (Set (y))
equal (Set(x:xs)) (Set(y))
  | member x (Set(y)) == True   = equal (Set(xs)) (Set(y))
  | member x (Set(y)) == False  = False
  | otherwise                   = True

--saferemove removes the given item from a set
saferemove :: Char -> Set -> Maybe Set
saferemove n (Set []) = Nothing
saferemove n (Set xs) = Just (Set([x | x <-xs, x /= n]))

--union takes a set and returns the union of both sets
union :: Set -> Set -> Set
union (Set []) (Set [])      = EmptySet
union (Set (x:xs)) (Set [])  = (Set (x:xs))
union (Set []) (Set (y:ys))  = (Set (y:ys))
union (Set (x:xs)) (Set (y:ys)) = mergeSets (Set (x:xs)) (Set(y:ys))

mergeSets :: Set -> Set -> Set
mergeSets EmptySet EmptySet  = EmptySet
mergeSets (Set x)  EmptySet  = (Set x)
mergeSets EmptySet (Set y)   = (Set y)
mergeSets (Set x) (Set y)    = Set (x++y)

--intersection takes two sets and returns the intersection of the two sets
intersection :: Set -> Set -> Set
intersection (Set []) (Set [])     = EmptySet
intersection (Set (x:xs)) (Set []) = EmptySet
intersection (Set []) (Set(y:ys))  = EmptySet
intersection (Set xs) (Set ys)     = Set [y | y <- ys, elem y xs]
