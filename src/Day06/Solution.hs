module Day06.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

import qualified Data.Set as Set

solve :: IO ()
solve = do
    --input <- readFile "inputs/Day06/real.txt"
    putStrLn $ "\t* Part 1: <uncomment to run>"
    putStrLn $ "\t* Part 2: <uncomment to run>"
    --putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    --putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = moveAndCount grid startPos North Set.empty
  where
    grid = lines input
    startPos = findStart grid

data Direction = North | East | South | West deriving (Eq, Ord, Show)

findStart :: [String] -> (Int, Int)
findStart grid = head [(row, col) | (row, line) <- zip [0..] grid,
                                   (col, char) <- zip [0..] line,
                                   char == '^']

moveAndCount :: [String] -> (Int, Int) -> Direction -> Set.Set (Int, Int) -> Int
moveAndCount grid pos@(_row, _col) dir visited = 
    case () of
        _ | outOfBounds newPos grid -> Set.size (Set.insert pos visited)
          | isBlocked newPos grid -> moveAndCount grid pos (turnRight dir) visited
          | otherwise -> moveAndCount grid newPos dir (Set.insert pos visited)
    where
        newPos = move pos dir

move :: (Int, Int) -> Direction -> (Int, Int)
move (row, col) dir = case dir of
    North -> (row - 1, col)
    East  -> (row, col + 1)
    South -> (row + 1, col)
    West  -> (row, col - 1)

turnRight :: Direction -> Direction
turnRight dir = case dir of
    North -> East
    East  -> South
    South -> West
    West  -> North

outOfBounds :: (Int, Int) -> [String] -> Bool
outOfBounds (row, col) grid = 
    row < 0 || row >= length grid || col < 0 || col >= length (head grid)

isBlocked :: (Int, Int) -> [String] -> Bool
isBlocked (row, col) grid = 
    (grid !! row) !! col == '#'

solveFuncPart2 :: String -> Int
solveFuncPart2 input = length loopPositions
  where
    grid = lines input
    startPos = findStart grid
    emptyPositions = [(r,c) | r <- [0..length grid - 1]
                           , c <- [0..length (head grid) - 1]
                           , (grid !! r) !! c == '.'
                           , (r,c) /= startPos]
    loopPositions = filter (createsLoop grid startPos) emptyPositions

createsLoop :: [String] -> (Int, Int) -> (Int, Int) -> Bool
createsLoop grid startPos obstaclePos = detectLoop grid' startPos North Set.empty Set.empty
  where
    grid' = addObstacle grid obstaclePos

addObstacle :: [String] -> (Int, Int) -> [String]
addObstacle grid (r,c) = 
    take r grid ++ 
    [take c (grid !! r) ++ "#" ++ drop (c+1) (grid !! r)] ++ 
    drop (r+1) grid

detectLoop :: [String] -> (Int, Int) -> Direction -> Set.Set (Int, Int, Direction) -> Set.Set (Int, Int) -> Bool
detectLoop grid pos@(row, col) dir visited positions
    | outOfBounds newPos grid = False
    | isBlocked newPos grid = detectLoop grid pos (turnRight dir) visited' positions'
    | state `Set.member` visited = True
    | otherwise = detectLoop grid newPos dir visited' positions'
  where
    newPos = move pos dir
    state = (row, col, dir)
    visited' = Set.insert state visited
    positions' = Set.insert pos positions
