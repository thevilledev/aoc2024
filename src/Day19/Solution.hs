{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day19.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Data.List.Split (splitOn)

-- Represents a single stripe color
type Color = Char

-- Represents a pattern of stripes
type Pattern = [Color]

-- Represents a count of possible combinations
type Count = Int

-- Input parsing result
data TowelData = TowelData
    { availablePatterns :: [Pattern]
    , desiredDesigns :: [Pattern]
    }

solve :: IO ()
solve = do
    input <- readFile "inputs/Day19/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    --putStrLn "\t* Part 2: <uncomment to run>"
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

-- Parse input into TowelData
parseInput :: String -> TowelData
parseInput input = TowelData pats des
  where
    parts = splitOn "\n\n" input
    (patternsStr, designsStr) = case parts of
      [p, d] -> (p, d)
      _ -> error "Invalid input format: expected patterns and designs separated by blank line"
    pats = map trim $ splitOn "," patternsStr
    des = lines designsStr
    trim = filter (/= ' ')

-- Check if a design can be made from available patterns
canMakeDesign :: [Pattern] -> Pattern -> Bool
canMakeDesign patterns design = dp !! length design
  where
    dp = True : [canMakePrefix i | i <- [1..length design]]
    canMakePrefix i = any (\p -> canUsePattern p i) patterns
    canUsePattern p i = 
      let plen = length p
      in i >= plen && 
         p == take plen (drop (i-plen) design) && 
         dp !! (i-plen)

-- Main solution function
solveFuncPart1 :: String -> Int
solveFuncPart1 input = length $ filter id $ map (canMakeDesign pats) des
  where
    TowelData pats des = parseInput input

-- Count all ways to make a design from patterns
countDesignWays :: [Pattern] -> Pattern -> Count
countDesignWays patterns design = last dp
  where
    dp = 1 : [waysToMakePrefix i | i <- [1..length design]]

    waysToMakePrefix i = sum [waysUsingPattern p i | p <- patterns]

    waysUsingPattern p i =
      let plen = length p
      in if i >= plen && 
            p == take plen (drop (i-plen) design)
         then dp !! (i-plen)
         else 0

-- Main solution function for part 2
solveFuncPart2 :: String -> Int
solveFuncPart2 input = sum $ map (countDesignWays pats) des
  where
    TowelData pats des = parseInput input
