module Day05.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

import Data.List (elemIndex, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map

-- Main function to solve both parts
solve :: IO ()
solve = do
    input <- readFile "inputs/Day05/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

-- Solve Part 1: Sum of middle numbers of valid updates
solveFuncPart1 :: String -> Int
solveFuncPart1 input = sum $ map findMiddle validUpdates
  where
    (rulesSection, updatesSection) = break null $ lines input
    rules = buildRuleMap rulesSection
    updates = map parseUpdate $ dropWhile null updatesSection
    validUpdates = filter (isValidUpdate rules) updates

-- Solve Part 2: Sum of middle numbers of reordered invalid updates
solveFuncPart2 :: String -> Int
solveFuncPart2 input = sum $ map findMiddle reorderedUpdates
  where
    (rulesSection, updatesSection) = break null $ lines input
    rules = buildRuleMap rulesSection
    updates = map parseUpdate $ dropWhile null updatesSection
    invalidUpdates = filter (not . isValidUpdate rules) updates
    reorderedUpdates = map (reorderUpdate rules) invalidUpdates

-- Build a map of rules from the rules section
buildRuleMap :: [String] -> Map Int [Int]
buildRuleMap = foldr addRule Map.empty
  where
    addRule line acc = case map read $ wordsWhen (=='|') line of
        [a, b] -> Map.insertWith (++) a [b] acc
        _      -> acc

-- Reorder an update according to the rules
reorderUpdate :: Map Int [Int] -> [Int] -> [Int]
reorderUpdate rules = sortBy compareByRules
  where
    compareByRules a b = case (mustComeBefore a b, mustComeBefore b a) of
        (True, False) -> LT
        (False, True) -> GT
        _            -> EQ

    mustComeBefore x y = case Map.lookup x rules of
        Nothing -> False
        Just afters -> y `elem` afters

-- Parse a line of update into a list of integers
parseUpdate :: String -> [Int]
parseUpdate = map read . wordsWhen (==',')

-- Check if an update is valid according to the rules
isValidUpdate :: Map Int [Int] -> [Int] -> Bool
isValidUpdate rules update = all checkRule (Map.toList rules)
  where
    checkRule (before, afters) = case elemIndex before update of
        Nothing -> True
        Just beforeIdx -> all (\after -> 
            case elemIndex after update of
                Nothing -> True
                Just afterIdx -> beforeIdx < afterIdx
            ) afters

-- Find the middle number in a list
findMiddle :: [Int] -> Int
findMiddle nums = nums !! (length nums `div` 2)

-- Split a string into words based on a predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                  "" -> []
                  s' -> w : wordsWhen p s''
                        where (w, s'') = break p s'
