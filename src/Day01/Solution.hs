module Day01.Solution
    ( solve,
      solveFuncPart1,
      solveFuncPart2
    ) where

import Data.List (sort)

solve :: IO ()
solve = do
    input' <- readFile "inputs/Day01/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input')
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input')

solveFuncPart1 :: String -> Int
solveFuncPart1 input = sum $ zipWith (\x y -> abs(x-y)) sortedCol1 sortedCol2
  where
    -- Parse input lines into pairs of numbers
    -- Pay attention to the input format and non-exhaustive pattern matching
    pairs = map ( safeParse . words) $ lines input
      where
        safeParse [a, b] = (read a, read b)
        safeParse xs = error $ "Invalid input, expected 2 numbers, got: " ++ show (length xs)
    -- Split into columns and sort them
    sortedCol1 = sort $ map fst pairs
    sortedCol2 = sort $ map snd pairs

solveFuncPart2 :: String -> Int
solveFuncPart2 input = sum $ map (\x -> x * (frequency x col2)) col1
  where
    -- Parse input lines into pairs of numbers
    pairs = map (safeParse . words) $ lines input
      where
        safeParse [a, b] = (read a, read b)
        safeParse xs = error $ "Invalid input, expected 2 numbers, got: " ++ show (length xs)
    
    col1 = map fst pairs
    col2 = map snd pairs
    
    -- Helper function to count frequency of a number in a list
    frequency n = length . filter (== n)