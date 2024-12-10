module Day10.Solution
    ( solve
     , solveFuncPart1
     , solveFuncPart2
    ) where

import Data.List (nub)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day10/real.txt"
    input' <- readFile "inputs/Day10/simple.txt"
    putStrLn $ "\t* Part 1 (simple): " ++ show (solveFuncPart1 input')
    putStrLn $ "\t* Part 1 (real): " ++ show (solveFuncPart1 input)
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = sum [findPaths grid pos | pos <- startPositions grid]
  where
    grid = parseInput input

type Position = (Int, Int)
type Grid = [[Int]]

parseInput :: String -> Grid
parseInput = map (map (read . pure)) . lines

startPositions :: Grid -> [Position]
startPositions grid = [(r, c) | 
    r <- [0..length grid - 1], 
    c <- [0..length (head grid) - 1], 
    grid !! r !! c == 0]

findPaths :: Grid -> Position -> Int
findPaths grid pos = 
    length $ nub $ map last $ findPathsHelper grid pos []
  where
    findPathsHelper :: Grid -> Position -> [Position] -> [[Position]]
    findPathsHelper g (r, c) visited = 
        let currentHeight = getValue g (r, c)
            nextHeight = currentHeight + 1
            path = reverse ((r,c):visited)
        in if currentHeight == 9
           then [path]
           else if length visited >= 9
           then []
           else concat [
                findPathsHelper g nextPos ((r,c):visited) |
                nextPos <- getNeighbors g (r, c),
                getValue g nextPos == nextHeight,
                nextPos `notElem` visited]

getNeighbors :: Grid -> Position -> [Position]
getNeighbors grid (r, c) = 
    filter (isValid grid) [
        (r-1, c),  -- North
        (r+1, c),  -- South
        (r, c-1),  -- West
        (r, c+1)]  -- East

isValid :: Grid -> Position -> Bool
isValid grid (r, c) = r >= 0 && r < length grid && 
                      c >= 0 && c < length (head grid)

getValue :: Grid -> Position -> Int
getValue grid (r, c) = grid !! r !! c

solveFuncPart2 :: String -> Int
solveFuncPart2 input = sum [countPaths grid pos | pos <- getTrailheads grid]
  where
    grid = parseInput input

countPaths :: Grid -> Position -> Int
countPaths grid pos = 
    findPathsHelper grid pos []
  where
    findPathsHelper :: Grid -> Position -> [Position] -> Int
    findPathsHelper g (r, c) visited = 
        let currentHeight = getValue g (r, c)
            nextHeight = currentHeight + 1
        in if currentHeight == 9
           then 1
           else sum [
                findPathsHelper g nextPos ((r,c):visited) |
                nextPos <- getNeighbors g (r, c),
                getValue g nextPos == nextHeight,
                nextPos `notElem` visited]

getTrailheads :: Grid -> [Position]
getTrailheads grid = [(r, c) | 
    r <- [0..length grid - 1], 
    c <- [0..length (head grid) - 1], 
    grid !! r !! c == 0]
