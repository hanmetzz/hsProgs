{- ##############
   Hannah Metzler
     Prog7.hs
     11/21/19
  ##############-}

module Prog7 where

--unique returns a list of elements that occur exactly once in the arg list
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) 
  | x `elem` xs   = unique (helper x xs)
  | otherwise     = x:(unique(helper x xs))

helper :: Eq a => a -> [a] -> [a]
helper x [] = []
helper x (y:ys)
  | x == y   = helper x ys
  | otherwise= y:(helper x ys)

--create Expr1
data Expr1 = Val1 Int
            | Add1 Expr1 Expr1
            | Sub1 Expr1 Expr1
          deriving Show

--value1 evaluates an expression
value1 :: Expr1 -> Int
value1 (Val1 n) = n
value1 (Add1 a1 a2) = (value1 a1) + (value1 a2)
value1 (Sub1 s1 s2) = (value1 s1) - (value1 s2)
--created expr2 to also support multiplication and division
data Expr2 = Val2 Int
           | Add2 Expr2 Expr2
           | Sub2 Expr2 Expr2
           | Div2 Expr2 Expr2
           | Mul2 Expr2 Expr2

--value2 evaluates an expression but returns Nothing ifyou div by 0
value2 :: Expr2 -> Maybe Int
value2 (Val2 n) = (Just n)
value2 (Add2 a1 a2) = Just (helper2 (value2 a1) + helper2 (value2 a2))
value2 (Sub2 s1 s2) = Just (helper2 (value2 s1) - helper2 (value2 s2))
value2 (Mul2 m1 m2) = Just (helper2 (value2 m1) * helper2 (value2 m2))
value2 (Div2 d1 d2)
  | helper2 (value2 d2) == 0  = Nothing
  | otherwise             = Just ((helper2 (value2 d1)) `div` (helper2 (value2 d2)))

helper2 :: Maybe Int -> Int
helper2 (Just n)  = n
helper2 (Nothing) = 0

--making expr2 and instance of show class to do more complex equations
instance Show Expr2 where
  show (Val2 n) = show n
  show (Add2 a1 a2) = "(" ++ show a1 ++ "+" ++ show a2 ++ ")"
  show (Sub2 s1 s2) = "(" ++ show s1 ++ "-" ++ show s2 ++ ")"
  show (Mul2 m1 m2) = "(" ++ show m1 ++ "*" ++ show m2 ++ ")"
  show (Div2 d1 d2) = "(" ++ show d1 ++ "/" ++ show d2 ++ ")"

--piglatinize returns word in piglatin form - vowel "yay", else "ay"
piglatinize :: String -> String
piglatinize [] = ""
piglatinize str = if (hasVowel (head str))
               then str ++ "yay"
                    else if (not (hasVowel (head(tail(str)))))
                    then (tail(tail str)) ++ [(head str)] ++ [(head(tail str))] ++ "ay"
                         else (tail str) ++ [(head str)] ++ "ay"

hasVowel :: Char -> Bool
hasVowel x = x `elem` ['a','e','i','o','u','A','E','I','O','U']

--balanced returns whether a tree is balanced or not
data Tree a = Leaf a | Node (Tree a) (Tree a)

balanced :: Tree a -> Bool
balanced (Leaf n) = True
balanced (Node left right)
  | (numLeaves left) - (numLeaves right) == 1 = True
  | (numLeaves left) - (numLeaves right) == -1 = True
  | (numLeaves left) - (numLeaves right) == 0 = True
  | otherwise         = False

numLeaves :: Tree a -> Int
numLeaves (Leaf n) = 1
numLeaves (Node left right) = numLeaves left + numLeaves right

--creating Expr3 that extends Expr and Expr2
data Expr3 = Val3 Int
           | Add3 Expr3 Expr3
           | Sub3 Expr3 Expr3
           | Div3 Expr3 Expr3
           | Mul3 Expr3 Expr3
           | If BExpr3 Expr3 Expr3
        deriving Show

data BExpr3 = BoolLit Bool
            |Or BExpr3 BExpr3
            |EqualTo BExpr3 BExpr3
            |LessThan BExpr3 BExpr3
        deriving Show

--bEval evaluates instance of above boolean expression
bEval :: BExpr3 -> Bool
bEval (BoolLit True) = True
bEval (BoolLit False) = False
bEval (Or b1 b2)= if ((bEval b1 || bEval b2) == True) then True else False
bEval (EqualTo b1 b2)= if (bEval b1 == bEval b2) then True else False
bEval (LessThan b1 b2)= if (bEval b1 < bEval b2) then True else False

--value3 evaluates an expression
value3 :: Expr3 -> Maybe Int
value3 (Val3 n) = (Just n)
value3 (Add3 a1 a2) = Just (helper2 (value3 a1) + helper2 (value3 a2))
value3 (Sub3 s1 s2) = Just (helper2 (value3 s1) + helper2 (value3 s2))
value3 (Mul3 m1 m2) = Just (helper2 (value3 m1) + helper2 (value3 m2))
value3 (Div3 d1 d2)
  | helper2 (value3 d2) == 0 = Nothing
  | otherwise  = Just (helper2 (value3 d1) `div` helper2 (value3 d2))
value3 (If b d1 d2)
  |bEval b = Just(helper2 (value3 d1))
  |otherwise = Just(helper2 (value3 d2))
