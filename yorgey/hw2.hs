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
exactMatches [x] [y] =
    if x == y
        then 1
        else 0
exactMatches (x:xs) (y:ys) = exactMatches [x] [y] + exactMatches xs ys
exactMatches _ _ = 0

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors x = map countColor colors
    where   countColor color = count color x
            count var = length . filter (var==)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = sum $ zipWith min ( countColors xs ) ( countColors ys ) 

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess em ( m - em )
    where   em = exactMatches secret guess
            m = matches secret guess 

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move move ex inex) code = ( exactMatches move code == ex ) && ( inexactMatches move code == inex )
    where   inexactMatches x y = ( matches x y ) - ( exactMatches x y )

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter ( isConsistent move ) codes

-- Exercise 6 -----------------------------------------

permute :: Int -> [a] -> [[a]]
permute 0 _ = []
permute 1 xs = map (\x->[x]) xs
permute n xs = [ x : y | x <- xs, y <- permute ( n - 1 ) xs ]

allCodes :: Int -> [Code]
allCodes codeLen = permute codeLen colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secret = solve' $ map ( getMove secret ) ( allCodes 4 )
    where   solve' ((Move code a b):moves) = Move code a b : solve' ( filter ( \x -> not $ isConsistent x code ) moves )
            solve' x = x

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined