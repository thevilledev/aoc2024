module Day16.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as PQ
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import System.IO.Unsafe (unsafePerformIO)

data Position = Position Int Int deriving (Eq, Ord, Show)
data Direction = North | East | South | West deriving (Eq, Ord, Show)
data State = State Position Direction Int deriving (Eq, Ord, Show)
data PathState = PathState {
    pos :: Position,
    dir :: Direction,
    score :: Int,
    path :: [Position]
} deriving (Eq, Ord, Show)

type Maze = Map.Map Position Char

solve :: IO ()
solve = do
    input <- readFile "inputs/Day16/real.txt"
    ---putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    part2Result <- solveFuncPart2 input
    putStrLn $ "\t* Part 2: " ++ show part2Result

solveFuncPart1 :: String -> Int
solveFuncPart1 input = 
    let maze = parseMaze input
        start = findChar 'S' maze
        end = findChar 'E' maze
    in dijkstra maze start end

parseMaze :: String -> Maze
parseMaze input = Map.fromList [((Position x y), c) 
                              | (y, line) <- zip [0..] (lines input)
                              , (x, c) <- zip [0..] line]

findChar :: Char -> Maze -> Position
findChar target maze = fst $ head $ filter ((== target) . snd) $ Map.toList maze

moveForward :: Direction -> Position -> Position
moveForward dir (Position x y) = case dir of
    North -> Position x (y-1)
    South -> Position x (y+1)
    East  -> Position (x+1) y
    West  -> Position (x-1) y

turnLeft :: Direction -> Direction
turnLeft dir = case dir of
    North -> West
    West  -> South
    South -> East
    East  -> North

turnRight :: Direction -> Direction
turnRight dir = case dir of
    North -> East
    East  -> South
    South -> West
    West  -> North

possibleMoves :: Maze -> State -> [(Int, State)]
possibleMoves maze (State pos dir score) =
    let forward = moveForward dir pos
        left = moveForward (turnLeft dir) pos
        right = moveForward (turnRight dir) pos
        isValid p = Map.findWithDefault '#' p maze /= '#'
        moves = []
            ++ [(1, State forward dir (score + 1)) | isValid forward]
            ++ [(1001, State left (turnLeft dir) (score + 1001)) | isValid left]
            ++ [(1001, State right (turnRight dir) (score + 1001)) | isValid right]
    in moves

dijkstra :: Maze -> Position -> Position -> Int
dijkstra maze start end = go initQueue initSeen
  where
    initState = State start East 0
    initQueue = PQ.singleton 0 initState
    initSeen = Set.empty

    go queue seen = case PQ.minViewWithKey queue of
        Nothing -> maxBound
        Just ((_, State pos dir score), rest) 
            | pos == end -> score
            | State pos dir score `Set.member` seen -> go rest seen
            | otherwise -> 
                let seen' = Set.insert (State pos dir score) seen
                    moves = possibleMoves maze (State pos dir score)
                    queue' = foldr (\(cost, state) q -> PQ.insert (score + cost) state q) rest moves
                in go queue' seen'

solveFuncPart2 :: String -> IO Int
solveFuncPart2 input = do
    let maze = parseMaze input
        start = findChar 'S' maze
        end = findChar 'E' maze
        minScore = dijkstra maze start end
        bestPaths = findBestPaths maze start end minScore
    debugPrintMaze maze bestPaths
    return $ Set.size bestPaths

findBestPaths :: Maze -> Position -> Position -> Int -> Set.Set Position
findBestPaths maze start end targetScore = go initQueue Map.empty Set.empty
  where
    initState = PathState start East 0 [start]
    initQueue = PQ.singleton 0 initState

    go queue bestScores bestPaths = case PQ.minViewWithKey queue of
        Nothing -> Set.insert start $ Set.insert end bestPaths
        Just ((_, PathState pos dir score path), rest)
            | score > targetScore -> go rest bestScores bestPaths
            | pos == end && score == targetScore -> 
                go rest bestScores (Set.union bestPaths (Set.fromList path))
            | length path > 100 -> go rest bestScores bestPaths
            | otherwise -> 
                let currentBest = Map.findWithDefault Map.empty pos bestScores
                    dirScore = Map.findWithDefault maxBound dir currentBest
                    shouldExplore = score <= dirScore
                    bestScores' = if shouldExplore 
                                then Map.insert pos (Map.insert dir score currentBest) bestScores
                                else bestScores
                    moves = possibleMoves maze (State pos dir score)
                    nextStates = [(cost, PathState newPos newDir newScore (newPos:path)) 
                                | (cost, State newPos newDir newScore) <- moves,
                                  newScore <= targetScore,
                                  let newPosBest = Map.findWithDefault Map.empty newPos bestScores,
                                  let newDirScore = Map.findWithDefault maxBound newDir newPosBest,
                                  newScore <= newDirScore || newPos == end]
                    queue' = if shouldExplore
                            then foldr (\(cost, state) q -> PQ.insert (score + cost) state q) rest nextStates
                            else rest
                in go queue' bestScores' bestPaths

-- Helper function to check if a position can potentially reach the end with remaining score
canReachEndWithScore :: Maze -> Position -> Position -> Int -> Bool
canReachEndWithScore maze (Position x1 y1) (Position x2 y2) remainingScore =
    let minSteps = abs (x2 - x1) + abs (y2 - y1)  -- Manhattan distance
        minRotations = if x1 == x2 || y1 == y2 then 0 else 1  -- Need at least one turn if not aligned
        minScore = minSteps + (minRotations * 1001)
    in minScore <= remainingScore

debugPrintMaze :: Maze -> Set.Set Position -> IO ()
debugPrintMaze maze paths = do
    let positions = Map.keys maze
    let maxX = maximum $ map (\(Position x _) -> x) positions
    let maxY = maximum $ map (\(Position _ y) -> y) positions
    putStrLn "Found paths:"
    forM_ [0..maxY] $ \y -> do
        forM_ [0..maxX] $ \x -> do
            let pos = Position x y
            let c = Map.findWithDefault '#' pos maze
            if c == '#' 
                then putChar '#'
                else if Set.member pos paths
                    then putChar 'O'
                    else putChar '.'
        putStrLn ""
    putStrLn ""
