{- ##############
 Hannah  Metzler
     CSC345
     Prog1
     9-5-19
############### -}

module Prog1 where

--isSingleDigit returns whether an integer number is between -10 and 10
isSingleDigit :: Integer -> Bool
isSingleDigit x
  | x < 10 && x > -10   = True
  | otherwise           = False

--dividesEvenly returns whether some dividend is evenly divisible by a divisor
dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly a b = if (a `mod` b == 0) then True else False

--middle returns the middle of three arguments
middle :: Integer -> Integer -> Integer -> Integer
middle x y z 
  | (x >= y && x <= z) || (x <= y && x >= z) = x
  | (y >= x && y <= z) || (y <= x && y >= z) = y
  | otherwise                                = z
--nand will return true if both inputs are false
nand :: Bool -> Bool -> Bool
nand f g
  | f == True && g == True      = False
  | otherwise                   = True

--triangleArea computes the area of a triangle given base and height
triangleArea :: Integer -> Integer -> Float
triangleArea b h = fromIntegral (b * h) / 2

--floorDecimal calculates the floor of a float
floorDecimal :: Float -> Float
floorDecimal x = fromIntegral(floor x) 

--isNotALetter returns whether a given character is a letter or not
isNotALetter :: Char -> Bool
isNotALetter x
  | x >= 'a' && x <= 'z'         = False
  | x >= 'A' && x <= 'Z'         = False
  | otherwise                    = True

--letterGrade is a function that will return the letter grade from a # grade
letterGrade :: Integer -> String
letterGrade x
  |x >= 93             = "A"
  |x < 93 && x >= 90   = "A-"
  |x < 90 && x >= 87   = "B+"
  |x < 87 && x >= 83   = "B"
  |x < 83 && x >= 80   = "B-"
  |x < 80 && x >= 77   = "C+"
  |x < 77 && x >= 73   = "C"
  |x < 73 && x >= 70   = "C-"
  |x < 70 && x >= 67   = "D+"
  |x < 67 && x >= 63   = "D"
  |x < 63 && x >= 60   = "D-"
  |x < 60              = "F"

--averageThree is a function that returns the average of three integers
averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = (fromIntegral x + fromIntegral y + fromIntegral z) /3

--howManyBelowAverage returns how many of 3 integers are below the avg
isBelowAverage  :: Integer -> Float -> Integer
isBelowAverage x avg
  | avg == 0              = 0
  | avg >= fromIntegral x = 1
  | otherwise             = 0

howManyBelowAverage :: Integer -> Integer -> Integer -> Integer
howManyBelowAverage x y z = isBelowAverage x avg + isBelowAverage y avg + isBelowAverage z avg
  where avg = averageThree x y z

