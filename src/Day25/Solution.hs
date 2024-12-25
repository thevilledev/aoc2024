module Day25.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Data.List (transpose)
import Data.List.Split (splitOn)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day25/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

type ColumnHeight = Int
type Lock = [ColumnHeight]
type Key = [ColumnHeight]

-- Count consecutive '#' characters from start until '.'
countUntilDot :: [Char] -> Int
countUntilDot = length . takeWhile (== '#')

-- Parse a single schematic into column heights
parseSchematic :: [String] -> Either Lock Key
parseSchematic rows
    | all (== '#') (head rows) = Left $ map (countUntilDot . tail) (transpose rows)  -- Skip first row, count from second row down
    | all (== '#') (last rows) = Right $ map (countUntilDot . reverse . init) (transpose rows)  -- Skip last row, count from second-to-last row up
    | otherwise = error "Invalid schematic: neither lock nor key pattern"

-- Parse all schematics from input
parseInput :: String -> ([Lock], [Key])
parseInput input = foldr classifySchematic ([], []) schematics
  where
    schematics = map lines $ filter (not . null) $ splitOn "\n\n" input
    classifySchematic schematic (locks, keys) = case parseSchematic schematic of
        Left lock -> (lock:locks, keys)
        Right key -> (locks, key:keys)

-- Check if a lock and key are compatible
isCompatible :: Lock -> Key -> Bool
isCompatible lock key = all (<= 5) $ zipWith (+) lock key

-- Count all compatible lock-key pairs
countCompatiblePairs :: [Lock] -> [Key] -> Int
countCompatiblePairs locks keys = length [(l, k) | l <- locks, k <- keys, isCompatible l k]

solveFuncPart1 :: String -> Int
solveFuncPart1 input = 
    let (locks, keys) = parseInput input
    in countCompatiblePairs locks keys

solveFuncPart2 :: String -> Int
solveFuncPart2 _ = 0  -- Part 2 not needed for this puzzle
