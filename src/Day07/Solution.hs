module Day07.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

solve :: IO ()
solve = do
    putStrLn $ "\t* Part 1: <uncomment to run>"
    putStrLn $ "\t* Part 2: <uncomment to run>"
    --input <- readFile "inputs/Day07/real.txt"
    --putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    --putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = sum $ map testValue validEquations
  where
    equations = parseInput input
    validEquations = filter canBeSolved equations

type Equation = (Int, [Int])

parseInput :: String -> [Equation]
parseInput = map parseLine . lines
  where
    parseLine line = 
      let (test:nums) = map read $ words $ filter (`notElem` ":") line
      in (test, nums)

canBeSolved :: Equation -> Bool
canBeSolved (target, nums) = any (\ops -> target == evaluate ops nums) (allCombinations (length nums - 1))

evaluate :: [Char] -> [Int] -> Int
evaluate ops nums = foldl apply (head nums) (zip (tail nums) ops)
  where
    apply acc (n, '+') = acc + n
    apply acc (n, '*') = acc * n

allCombinations :: Int -> [[Char]]
allCombinations n = sequence $ replicate n ['+', '*']

testValue :: Equation -> Int
testValue (test, _) = test

-- Part 2 specific functions
canBeSolvedWithConcat :: Equation -> Bool
canBeSolvedWithConcat (target, nums) = any (\ops -> target == evaluateWithConcat ops nums) (allCombinationsWithConcat (length nums - 1))

evaluateWithConcat :: [Char] -> [Int] -> Int
evaluateWithConcat ops nums = foldl apply (head nums) (zip (tail nums) ops)
  where
    apply acc (n, '+') = acc + n
    apply acc (n, '*') = acc * n
    apply acc (n, '|') = read (show acc ++ show n)

allCombinationsWithConcat :: Int -> [[Char]]
allCombinationsWithConcat n = sequence $ replicate n ['+', '*', '|']

solveFuncPart2 :: String -> Int
solveFuncPart2 input = sum $ map testValue validEquations
  where
    equations = parseInput input
    validEquations = filter canBeSolvedWithConcat equations
