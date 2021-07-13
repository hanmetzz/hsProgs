{- ###############
   Hannah Metzler
   CSC345 Prog4.hs
   10/22/19
  ############### -}

module Prog4 where

--moreRecent takes two dates and returns whichever one is more recent
moreRecent :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
moreRecent (x,y,z) (a,b,c)
  | c > z                     = (a,b,c)
  | z > c                     = (x,y,z)
  | z == c && a > x           = (a,b,c)
  | z == c && x > a           = (x,y,z)
  | z == c && a == x && b > y = (a,b,c)
  | z == c && a == x && y > b = (x,y,z)

--numInMonth takes a month and list of dates and returns num of dates in list
numInMonth :: [(Int,Int,Int)] -> Int -> Int
numInMonth [] n = 0
numInMonth xs n = length[(a,b,c) | (a,b,c) <- xs, a == n]

--datesInMonth takes a month and list and returns list of dates in month
datesInMonth :: [(Int,Int,Int)] -> Int -> [(Int,Int,Int)]
datesInMonth [] n = []
datesInMonth xs n = [(a,b,c) | (a,b,c) <- xs, a == n]

--month2Str takes a date and returns the month string
month2Str :: (Int,Int,Int) -> String
month2Str (x,y,z)
  | x == 1   = "January"
  | x == 2   = "February"
  | x == 3   = "March"
  | x == 4   = "April"
  | x == 5   = "May"
  | x == 6   = "June"
  | x == 7   = "July"
  | x == 8   = "August"
  | x == 9   = "September"
  | x == 10  = "October"
  | x == 11  = "November"
  | x == 12  = "December"

--date2Str returns string equivalent in month day,year
date2Str :: (Int,Int,Int) -> String
date2Str (a,b,c) = month2Str (a,b,c)++ " " ++ show b ++ ", " ++ show c

--monthLookup returns a numeric day in the calendar and returns month
monthLookup :: Int -> Int
monthLookup x 
  | x >= 1 && x <= 31   = 1
  | x >= 32 && x <= 59  = 2
  | x >= 60 && x <= 90  = 3
  | x >= 91 && x <= 120 = 4
  | x >= 121 && x <= 151 = 5
  | x >= 152 && x <= 181 = 6
  | x >= 182 && x <= 212 = 7
  | x >= 213 && x <= 243 = 8
  | x >= 244 && x <= 273 = 9
  | x >= 274 && x <= 304 = 10
  | x >= 305 && x <= 334 = 11
  | x >= 334 && x <= 365 = 12

--monthRange returns a list of months between 2 numeric days
monthRange :: Int -> Int -> [Int]
monthRange x y 
  | x > y      = [] 
  | otherwise  = [m | m <- [1..12], (monthLookup x) <= m, m <= (monthLookup y)]

--validDate takes a date and reruns whetherr it is valid
validDate :: (Int,Int,Int) -> Bool
validDate (x,y,z)
  | x > 12                                              = False
  | x == 2 && y >= 29                                    = False
  | (x == 4 || x == 6 || x == 9 || x == 11) && y > 30   = False
  | (x ==1 || x == 3 || x == 5 || x == 7 || x == 10 || x == 12) && y > 31 = False
  | otherwise                                           = True

--validLeapDate takes a date and returns whether or not it is a leap date
validLeapDate :: (Int,Int,Int) -> Bool
validLeapDate (x,y,z) = if x == 2 && y == 29 && z `mod` 4 == 0 || z `mod` 400 == 0 then True else False

--season takes a date and returns what season that date is in
season :: (Int,Int,Int) -> String
season (x,y,z)
  | (x == 12 && y >= 22) || x == 1 || x == 2 || (x == 3 && y <= 19)    = "Winter"
  | (x == 3 && y >= 20) || x == 4 || x == 5 || (x == 6 && y <= 20)     = "Spring"
  | (x == 6 && y >= 21) || x == 7 || x == 8 || (x == 9 && y <= 22)     = "Summer"
  | (x == 9 && y >= 23) || x == 10 || x == 11 || (x == 12 && y <= 21)  = "Fall"
