{- Hannah Metzler
Due Date: 3/22/19
Prog4.hs -}

module Prog4 where

--older takes two dates and returns whichever one is older
older :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
older (a, b, c) (x, y, z)
  |c > z        = (x, y, z)
  |z > c        = (a, b, c)
  |z == c && a > x = (x, y, z)
  |z == c && x > a = (a, b, c)
  |z == c && a == x && b > y = (x, y, z)
  |z == c && a == x && y > b = (a, b, c)

--numInMonth takes a month and list of dates and returns how many dates in the list match the given month
--numInMonth :: Int -> [(Int, Int, Int)] -> Int
--numInMonth y ((a,b,c):xs) = length [x | x <- xs, xs == a] 

--datesInMonth takes a month and a list of dates and returns a list of dates that match the given month
--datesInMonth :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]

--date2Str takes a date and returns its string equivalent
date2Str :: (Int, Int, Int) -> String
date2Str (x, y, z)
  |x == 1       = "January"
  |x == 2       = "February"
  |x == 3       = "March"
  |x == 4       = "April"
  |x == 5       = "May"
  |x == 6       = "June"
  |x == 7       = "July"
  |x == 8       = "August" 
  |x == 9       = "September"
  |x == 10      = "October"
  |x == 11      = "November"
  |x == 12      = "December"

--date2Str' returns string equivalent without 12 conditionals
date2Str' :: (Int, Int, Int) -> String
months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
date2Str' (x, y, z) = months !! (x - 1)

--monthLookup takes a numeric day and returns what month the day is in
monthLookup :: Int -> Int
monthLookup x
  |x >= 1 && x <= 31    = 1
  |x >= 32 && x <= 59   = 2
  |x >= 60 && x <= 90   = 3
  |x >= 91 && x <= 120  = 4
  |x >= 121 && x <= 151 = 5
  |x >= 152 && x <= 181 = 6
  |x >= 182 && x <= 212 = 7
  |x >= 213 && x <= 243 = 8
  |x >= 244 && x <= 273 = 9
  |x >= 274 && x <= 304 = 10
  |x >= 305 && x <= 334 = 11
  |x >= 335 && x <= 365 = 12
  
--monthRange takes 2 numeric days and truens an integer list of the months between those dates
monthRange :: Int -> Int -> [Int]
monthRange x y = [x+1 .. y-1]

--validDate takes a date and returns whether it is valid
validDate :: (Int, Int, Int) -> Bool
validDate (x, y, z)
  | x > 12                                              = False
  | x == 2 && y >= 29                                   = False
  | x == 4 || x == 6 || x == 9 || x == 11 && y > 30     = False
  | x == 1 || x == 3 || x == 5 || x == 7 || x == 10 || x == 12 && y > 31 = False
  |otherwise                                            = True
  
--validLeapDate takes a date and returns whether it is a leap date
validLeapDate :: (Int, Int, Int) -> Bool
validLeapDate (x,y,z) = if x == 2 && y == 29 && z `mod` 4 == 0 || z `mod` 400 == 0 then True else False

--season takes a date and returns the season that date is in
season :: (Int, Int, Int) -> String
season (x, y, z)
  |x == 12 && y >= 21   = "Winter"
  |x == 1 || x == 2     = "Winter"
  |x == 3 && y <= 20    = "Winter"
  |x == 3 && y >= 21    = "Spring"
  |x == 4 || x == 5     = "Spring"
  |x == 6 && y <= 20    = "Spring"
  |x == 6 && y >= 21    = "Summer"
  |x == 7 || x == 8     = "Summer"
  |x == 9 && y <= 20    = "Summer"
  |x == 9 && y >= 21    = "Fall"
  |x == 10 || x == 11   = "Fall"
  |x == 12 && y <= 21   = "Winter"
