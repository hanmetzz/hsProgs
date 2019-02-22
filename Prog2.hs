-- Hannah Metzler 
module Prog2 where

--fromIntegral :: Int -> Float
--round :: Float -> Int

--sum function adds all values from 1 to x
sum' :: Integer -> Integer
sum' 0 = 0
sum' x = x + sum' (x - 1)

--integerSqrt
integerSqrt :: Integer -> Integer
integerSqrt num1 = (floor(sqrt(num1))

--exponent'
exponent' :: Integer -> Integer -> Integer
exponent' b 0 = 1
exponent' b 1 = b
exponent' b e = b * exponent' b (e-1) 

--or'
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

--orderTriple
maxOfThree :: Integer -> Integer -> Integer -> Integer
maxOfThree m1 m2 m3 = if m1 >= m2 && m1 >= m3 then m1 else if m2 >= m1 && m2 >= m3 then m2 else m3

middleOfThree :: Integer -> Integer -> Integer -> Integer
middleOfThree n1 n2 n3 = if n1 >= n2 && n2 >= n3 then n2 else if n2 >= n1 && n1 >= n3 then n1 else n3

minOfThree :: Integer -> Integer -> Integer -> Integer
minOfThree o1 o2 o3 = if o1 >= o2 && o2 >= o3 then o3 else if o2 >= o1 && o3 >= o1 then o1 else o2

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (t1, t2, t3) =((maxOfThree t1 t2 t3), (middleOfThree t1 t2 t3), (minOfThree t1 t2 t3)) 

--swap
swap :: (Char, Char, Char, Char) -> (Char, Char, Char, Char)
swap (c1, c2, c3, c4) = (c4, c3, c2, c1)

--asciiNums

--matches
matches :: Integer -> [Integer] -> [Integer]
matches a1 a2 = [a3 | a3 <- a2, a3 == a1]

--element
element :: Integer -> [Integer] -> Bool
element e1 [] = False
element e1 (e2 : es)
  | e1 == e2	= True
  | otherwise = element e1 (es)
