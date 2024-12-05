module Day04.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

import Data.Maybe (mapMaybe)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day04/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = length (findXMAS (parseGrid input)) `div` 2

type Grid = [[Char]]
type Position = (Int, Int)
type Direction = (Int, Int)

-- Parse the input string into a 2D grid
parseGrid :: String -> Grid
parseGrid = lines

-- All possible directions including backwards
directions :: [Direction]
directions = [ (-1,-1), (-1,0), (-1,1)    -- Up-left,    Up,      Up-right
             , (0,-1),          (0,1)      -- Left,                Right
             , (1,-1),  (1,0),  (1,1)      -- Down-left,  Down,    Down-right
             ]

inBounds :: Grid -> Position -> Bool
inBounds grid (row, col) = 
    row >= 0 && row < length grid && 
    col >= 0 && col < length (head grid)

charAt :: Grid -> Position -> Maybe Char
charAt grid (row, col)
    | inBounds grid (row, col) = Just (grid !! row !! col)
    | otherwise = Nothing

-- Get word starting from position in specific direction
getWord :: Grid -> Position -> Direction -> Int -> String
getWord grid (row, col) (dRow, dCol) len = 
    mapMaybe (\i -> charAt grid (row + i*dRow, col + i*dCol)) [0..len-1]

-- Find all occurrences of a word in the grid, including backwards
findXMAS :: Grid -> [(Position, Direction, String)]
findXMAS grid = 
    [ (pos, dir, word)
    | row <- [0..length grid - 1]
    , col <- [0..length (head grid) - 1]
    , let pos = (row, col)
    , dir <- directions
    , let word = getWord grid pos dir 4
    , word == "XMAS" || word == "SAMX"  -- Check both forward and backward patterns
    ]

solveFuncPart2 :: String -> Int
solveFuncPart2 input = length $ findMASPatterns (parseGrid input)

-- Find all 3x3 X-MAS patterns in the grid (both diagonals must be MAS/SAM)
findMASPatterns :: Grid -> [Position]
findMASPatterns grid = 
    [ (row, col)
    | row <- [0..length grid - 3]  -- Need 3 rows for the 3x3 pattern
    , col <- [0..length (head grid) - 3]  -- Need 3 columns for the 3x3 pattern
    , let diagonal1 = [charAt grid (row, col), charAt grid (row+1, col+1), charAt grid (row+2, col+2)]
    , let diagonal2 = [charAt grid (row, col+2), charAt grid (row+1, col+1), charAt grid (row+2, col)]
    , isMASorSAM diagonal1 && isMASorSAM diagonal2
    ]
  where
    isMASorSAM chars = 
        chars == [Just 'M', Just 'A', Just 'S'] || 
        chars == [Just 'S', Just 'A', Just 'M']
