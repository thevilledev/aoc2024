module Day15.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Debug.Trace (trace)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day15/small.txt"
    putStrLn $ "\t* Part 1 (small): " ++ show (solveFuncPart1 input)
    input' <- readFile "inputs/Day15/large.txt"
    putStrLn $ "\t* Part 1 (large): " ++ show (solveFuncPart1 input')
    input'' <- readFile "inputs/Day15/real.txt"
    putStrLn $ "\t* Part 1 (real): " ++ show (solveFuncPart1 input'')
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart2 :: String -> Int
solveFuncPart2 _ = 0  -- TODO: Implement solution

type Position = (Int, Int)
type Grid = [[Char]]

solveFuncPart1 :: String -> Int
solveFuncPart1 input = calculateGPSSum $ simulateRobot grid moves
  where
    (grid, moves) = parseInput input

parseInput :: String -> (Grid, String)
parseInput input = (gridLines, filter (`elem` "^v<>") moves)
  where
    (gridLines, movesLines) = break null $ lines input
    moves = concat $ drop 1 movesLines

findRobot :: Grid -> Position
findRobot grid = head [(r, c) | (r, row) <- zip [0..] grid,
                               (c, cell) <- zip [0..] row,
                               cell == '@']

simulateRobot :: Grid -> String -> Grid
simulateRobot grid [] = grid
simulateRobot grid (move:moves) = 
    case tryMove grid robotPos (getDirection move) of
        Just (newGrid, _) -> 
            --trace ("\nAfter move " ++ [move] ++ ":\n" ++ gridToString newGrid) $
            simulateRobot newGrid moves
        Nothing -> simulateRobot grid moves
  where
    robotPos = findRobot grid

--gridToString :: Grid -> String
--gridToString = unlines

getDirection :: Char -> Position
getDirection '^' = (-1, 0)
getDirection 'v' = (1, 0)
getDirection '<' = (0, -1)
getDirection '>' = (0, 1)
getDirection _ = (0, 0)

tryMove :: Grid -> Position -> Position -> Maybe (Grid, Position)
tryMove grid (r, c) (dr, dc) =
    if canMove grid (r, c) (dr, dc)
    then Just (moveRobot grid (r, c) (dr, dc))
    else Nothing

isInBounds :: Grid -> Position -> Bool
isInBounds grid (r, c) = 
    r >= 0 && r < length grid && c >= 0 && c < length (head grid)

canMove :: Grid -> Position -> Position -> Bool
canMove grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
    in isInBounds grid (newR, newC) && 
       case grid !! newR !! newC of
           '.' -> True
           'O' -> canPushBoxChain grid (newR, newC) (dr, dc)
           _ -> False

canPushBoxChain :: Grid -> Position -> Position -> Bool
canPushBoxChain grid (r, c) (dr, dc) =
    let nextR = r + dr
        nextC = c + dc
    in if not (isInBounds grid (nextR, nextC)) || grid !! nextR !! nextC == '#'
       then False
       else case grid !! nextR !! nextC of
           '.' -> True
           'O' -> canPushBoxChain grid (nextR, nextC) (dr, dc)
           _ -> False

moveRobot :: Grid -> Position -> Position -> (Grid, Position)
moveRobot grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
        targetCell = grid !! newR !! newC
    in case targetCell of
        '.' -> (updateGrid (updateGrid grid (r, c) '.') (newR, newC) '@', (newR, newC))
        'O' -> 
            let (finalGrid, finalPos) = pushBoxes grid (newR, newC) (dr, dc)
            in if finalPos == (newR, newC)
               then (grid, (r, c))  -- Box couldn't be pushed
               else (updateGrid (updateGrid finalGrid (r, c) '.') (newR, newC) '@', (newR, newC))
        _ -> (grid, (r, c))

pushBoxes :: Grid -> Position -> Position -> (Grid, Position)
pushBoxes grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
    in if not (isInBounds grid (newR, newC)) || grid !! newR !! newC == '#'
       then (grid, (r, c))
       else case grid !! newR !! newC of
           'O' -> 
               -- Try to push the next box in chain
               let (nextGrid, finalPos) = pushBoxes grid (newR, newC) (dr, dc)
               in if finalPos == (newR, newC)
                  then (grid, (r, c))  -- If next box couldn't move, this one can't either
                  else 
                      -- Successfully pushed next box, now move this one
                      let withNextBoxMoved = nextGrid
                          withCurrentBoxMoved = updateGrid withNextBoxMoved (newR, newC) 'O'
                          finalGrid = updateGrid withCurrentBoxMoved (r, c) '.'
                      in (finalGrid, (newR, newC))
           '.' -> 
               -- Simple case: move box to empty space
               let withBoxMoved = updateGrid grid (newR, newC) 'O'
                   finalGrid = updateGrid withBoxMoved (r, c) '.'
               in (finalGrid, (newR, newC))
           _ -> (grid, (r, c))

updateGrid :: Grid -> Position -> Char -> Grid
updateGrid grid (r, c) newChar =
    take r grid ++
    [take c (grid !! r) ++ [newChar] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid

calculateGPSSum :: Grid -> Int
calculateGPSSum grid = sum [100 * r + c | 
    (r, row) <- zip [0..] grid,
    (c, cell) <- zip [0..] row,
    cell == 'O']
