module Day11.Solution
    ( solve
     , solveFuncPart1
     , solveFuncPart2
    ) where

solve :: IO ()
solve = do
    input <- readFile "inputs/Day11/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    --input' <- readFile "inputs/Day11/real2.txt"
    -- putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input')
    putStrLn "\t* Part 2: <run the python script>"

solveFuncPart1 :: String -> Int
solveFuncPart1 input = length $ iterate processStones initialStones !! 25
  where
    initialStones = map read (words input) :: [Integer]

processStones :: [Integer] -> [Integer]
processStones = concatMap transformStone

transformStone :: Integer -> [Integer]
transformStone n
    | n == 0 = [1]
    | even (length (show n)) = let (left, right) = splitNumber n in [left, right]
    | otherwise = [n * 2024]

splitNumber :: Integer -> (Integer, Integer)
splitNumber n = (read leftStr, read rightStr)
  where
    digits = show n
    (leftStr, rightStr) = splitAt (length digits `div` 2) digits

solveFuncPart2 :: String -> Int
solveFuncPart2 input = length $ iterate processStones initialStones !! 75
  where
    initialStones = map read (words input) :: [Integer]
