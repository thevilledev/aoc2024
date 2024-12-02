module Day01.Solution
    ( solve,
      solveFuncPart1,
      solveFuncPart2
    ) where

import Data.List (sort)
import qualified Data.Map as Map

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
solveFuncPart2 input = sum $ map (\x -> x * (Map.findWithDefault 0 x frequencyMap)) col1
  where
    -- Parse input lines into pairs and build frequency map simultaneously
    (pairs, frequencyMap) = foldr processLine ([], Map.empty) $ lines input
    
    processLine line (accPairs, accMap) =
        case words line of
            [a, b] -> let num1 = read a
                          num2 = read b
                     in ((num1, num2):accPairs, Map.insertWith (+) num2 1 accMap)
            xs -> error $ "Invalid input, expected 2 numbers, got: " ++ show (length xs)
    
    col1 = map fst pairs