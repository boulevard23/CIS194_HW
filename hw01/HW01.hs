{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = zipWith (\x y -> if (y `mod` 2 == 0) then x * 2 else x) l [(1 :: Integer)..]

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map f
  where f v
         | v <= 9    = v
         | otherwise = (sum (toRevDigits v))



-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = f . sumDigits . doubleEveryOther . toRevDigits
  where f v = v `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a c b
  | n <= 0 = []
  | otherwise = hanoi (n - 1) a b c ++
      ((a, c) : hanoi (n - 1) b c a)
