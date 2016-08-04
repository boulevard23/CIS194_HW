{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches (x:xs) (y:ys)
  | x == y = 1 + exactMatches xs ys
  | otherwise = exactMatches xs ys
exactMatches _ _ = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map count colors
    where count color = length $ filter (color==) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ zipWith min (countColors actual) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess exactMatchesCount (matchesCount - exactMatchesCount)
  where
    matchesCount = matches actual guess
    exactMatchesCount = exactMatches actual guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exactMatchesCount nonExactMatchesCount) anotherGuess = exactMatchesCount == exactMatchesCount' && nonExactMatchesCount == nonExactMatchesCount'
  where Move _ exactMatchesCount' nonExactMatchesCount' = getMove guess anotherGuess

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move allPossibilities = filter (isConsistent move) allPossibilities

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = [[]]
allCodes n = [c : codes | c <- colors, codes <- allCodes (n-1)]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
