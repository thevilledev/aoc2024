module Day18.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Sequence as Seq
import Data.Sequence ((><), ViewL(..))

type Coord = (Int, Int)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day18/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn "\t* Part 2: <uncomment to run>"
    --putStrLn $ "\t* Part 2: " ++ solveFuncPart2 input

-- Parse a single coordinate string like "5,4" into a tuple (5,4)
parseCoord :: String -> Coord
parseCoord s = case break (==',') s of
    (x, ',':y) -> (read x, read y)
    _ -> error "Invalid coordinate format"

-- Parse input into list of coordinates
parseInput :: String -> [Coord]
parseInput = map parseCoord . lines

-- Check if a coordinate is within bounds and not corrupted
isValidMove :: Coord -> Set Coord -> Bool
isValidMove (x, y) corrupted = 
    x >= 0 && x <= 70 && y >= 0 && y <= 70 && 
    not (Set.member (x, y) corrupted)

-- Get valid neighboring coordinates
getNeighbors :: Coord -> Set Coord -> [Coord]
getNeighbors (x, y) corrupted = 
    filter (`isValidMove` corrupted)
        [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

-- Find shortest path using BFS
shortestPath :: Set Coord -> Maybe Int
shortestPath corrupted = bfs initialState Set.empty
  where
    target = (70, 70)
    initialState = Seq.singleton ((0, 0), 0)  -- (position, steps)
    bfs queue visited = case Seq.viewl queue of
        Seq.EmptyL -> Nothing  -- No path found
        ((pos, steps) :< rest)
            | pos == target -> Just steps
            | Set.member pos visited -> bfs rest visited
            | otherwise -> 
                let neighbors = getNeighbors pos corrupted
                    newStates = [(n, steps + 1) | n <- neighbors, 
                                                 not (Set.member n visited)]
                    newQueue = rest >< Seq.fromList newStates
                in bfs newQueue (Set.insert pos visited)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = 
    let coords = take 1024 $ parseInput input
        corrupted = Set.fromList coords
    in fromMaybe (-1) $ shortestPath corrupted

-- Check if a path exists to target
hasPath :: Set Coord -> Bool
hasPath = isJust . shortestPath

-- Find first coordinate that makes path impossible
findBreakingPoint :: [Coord] -> Maybe Coord
findBreakingPoint coords = go coords Set.empty
  where
    go [] _ = Nothing
    go (c:cs) corrupted
      | hasPath (Set.insert c corrupted) = go cs (Set.insert c corrupted)
      | otherwise = Just c

-- Format coordinate as x,y string
formatResult :: Coord -> String
formatResult (x,y) = show x ++ "," ++ show y

solveFuncPart2 :: String -> String
solveFuncPart2 input = 
    let coords = parseInput input
    in maybe "No solution" formatResult $ findBreakingPoint coords
