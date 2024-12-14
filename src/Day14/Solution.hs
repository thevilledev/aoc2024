module Day14.Solution
    ( solve
    ,  solveFuncPart1
    ) where

import Data.List.Split (splitOn)

data Robot = Robot 
    { pos :: (Int, Int)  -- (x,y)
    , vel :: (Int, Int)  -- (dx,dy)
    } deriving (Show)

-- Parse a single line into a Robot
parseRobot :: String -> Robot
parseRobot line = case splitOn " " line of
    [pos', vel'] -> case (splitOn "," $ drop 2 pos', splitOn "," $ drop 2 vel') of
        ([x,y], [dx,dy]) -> Robot (read x, read y) (read dx, read dy)
        _ -> error "Invalid position/velocity format"
    _ -> error "Invalid robot format"

-- Move a single robot one step, handling wrapping
moveRobot :: Robot -> Robot
moveRobot (Robot (x,y) (dx,dy)) = 
    let newX = (x + dx) `mod` 101
        newY = (y + dy) `mod` 103
        -- Ensure we handle negative positions correctly
        wrappedX = if newX < 0 then newX + 101 else newX
        wrappedY = if newY < 0 then newY + 103 else newY
    in Robot (wrappedX, wrappedY) (dx,dy)

-- Simulate n steps for all robots
simulate :: Int -> [Robot] -> [Robot]
simulate 0 robots = robots
simulate n robots = simulate (n-1) (map moveRobot robots)

-- Check if a position is in a quadrant
inQuadrant :: (Int, Int) -> Int -> Bool
inQuadrant (x, y) quad = case quad of
    1 -> x < midX && x >= 0 && y < midY && y >= 0
    2 -> x > midX && x < 101 && y < midY && y >= 0
    3 -> x < midX && x >= 0 && y > midY && y < 103
    4 -> x > midX && x < 101 && y > midY && y < 103
    _ -> False
  where
    midX = 101 `div` 2  -- 50
    midY = 103 `div` 2  -- 51

-- Count robots in each quadrant and multiply
countQuadrants :: [Robot] -> Int
countQuadrants robots = 
    let counts = [length $ filter (\r -> inQuadrant (pos r) q) robots | q <- [1..4]]
    in product counts

solve :: IO ()
solve = do
    input <- readFile "inputs/Day14/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    --putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = 
    let robots = map parseRobot $ lines input
        finalPositions = simulate 100 robots
    in countQuadrants finalPositions
