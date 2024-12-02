module Day02.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

solve :: IO ()
solve = do
    input <- readFile "inputs/Day02/real.txt"
    putStrLn $ "\t*Part 1: " ++ show (solveFuncPart1 input)
    putStrLn $ "\t*Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = sum $ map calculateResult linesParsed
  where
    -- Parse the input into a list of lists of integers
    linesParsed = map (safeParse . words) $ lines input
      where
        -- Convert a list of strings to a list of integers
        safeParse xs = map read xs :: [Int]

    -- Calculate the result for each list of integers
    calculateResult xs
        -- If the sequence is valid increasing, return 1
        | isValidIncreasing xs = 1
        -- If the sequence is valid decreasing, return 1
        | isValidDecreasing xs = 1
        -- Otherwise, return 0
        | otherwise = 0

    -- Check if the sequence is strictly increasing with valid differences
    isValidIncreasing xs = isIncreasing xs && hasValidDifferences xs

    -- Check if the sequence is strictly decreasing with valid differences
    isValidDecreasing xs = isDecreasing xs && hasValidDifferences xs

    -- Check if all differences between consecutive numbers are between 1 and 3
    hasValidDifferences xs = and $ zipWith validDifference xs (tail xs)
      where
        -- Check if the difference between two numbers is between 1 and 3
        validDifference x y = let diff = abs (y - x) in diff >= 1 && diff <= 3

    -- Check if the list is strictly increasing
    isIncreasing xs = and $ zipWith (<) xs (tail xs)

    -- Check if the list is strictly decreasing
    isDecreasing xs = and $ zipWith (>) xs (tail xs)

solveFuncPart2 :: String -> Int
-- Main function that processes input and returns sum of valid sequences
solveFuncPart2 input = sum $ map calculateResult linesParsed
  where
    -- Parse input string into list of integer lists
    linesParsed = map (safeParse . words) $ lines input
      where
        safeParse xs = map read xs :: [Int]

    -- For each sequence, return 1 if it meets any validity criteria, 0 otherwise
    calculateResult xs
        -- Case 1: Sequence is already valid and increasing (e.g., [1,2,3])
        | isValidIncreasing xs = 1
        -- Case 2: Sequence is already valid and decreasing (e.g., [3,2,1])
        | isValidDecreasing xs = 1
        -- Case 3: Sequence can be made valid by removing one number
        | canBeMadeValid xs = 1
        -- Case 4: None of the above conditions met
        | otherwise = 0

    -- Checks if removing any single number can create a valid sequence
    canBeMadeValid xs = any isValidSequence (possibleSequences xs)
      where
        -- Generate all possible sequences by removing one number at a time
        possibleSequences nums = [take i nums ++ drop (i + 1) nums | i <- [0..length nums - 1]]
        -- Check if any resulting sequence is valid (either increasing or decreasing)
        isValidSequence nums = isValidIncreasing nums || isValidDecreasing nums

    -- A sequence is valid increasing if:
    -- 1. Numbers strictly increase (each number larger than previous)
    -- 2. Differences between consecutive numbers are between 1 and 3
    isValidIncreasing xs = isIncreasing xs && hasValidDifferences xs

    -- A sequence is valid decreasing if:
    -- 1. Numbers strictly decrease (each number smaller than previous)
    -- 2. Differences between consecutive numbers are between 1 and 3
    isValidDecreasing xs = isDecreasing xs && hasValidDifferences xs

    -- Checks if all consecutive number pairs have valid differences
    -- Valid difference: absolute difference between 1 and 3
    hasValidDifferences xs = and $ zipWith validDifference xs (tail xs)
      where
        validDifference x y = let diff = abs (y - x) in diff >= 1 && diff <= 3

    -- Checks if list is strictly increasing
    isIncreasing xs = and $ zipWith (<) xs (tail xs)

    -- Checks if list is strictly decreasing
    isDecreasing xs = and $ zipWith (>) xs (tail xs)