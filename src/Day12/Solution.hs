module Day12.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Data.Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace
import Data.List (sort, groupBy, find)

type Pos = (Int, Int)
type Grid = Array Pos Char
type Region = Set.Set Pos

solve :: IO ()
solve = do
    input <- readFile "inputs/Day12/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    input' <- readFile "inputs/Day12/simple.txt"
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input')

solveFuncPart1 :: String -> Int
solveFuncPart1 input = sum $ map calculateRegionPrice regions
  where
    grid = parseInput input
    regions = findRegions grid

-- solveFuncPart2 :: String -> Int
-- solveFuncPart2 input = sum $ map (calculateRegionPricePart2 grid) regions
--   where
--     grid = parseInput input
--    regions = findRegions grid

-- calculateRegionPricePartx :: Grid -> Region -> Int
-- calculateRegionPricePartx grid region = area * sides
--   where
--     area = Set.size region
    -- sides = countDistinctSides grid region

parseInput :: String -> Grid
parseInput input = array gridBounds $ concat 
    [[ ((row, col), char) 
     | (col, char) <- zip [0..] line] 
     | (row, line) <- zip [0..] (lines input)]
  where
    lns = lines input
    gridBounds = ((0, 0), (length lns - 1, length (head lns) - 1))

findRegions :: Grid -> [Region]
findRegions grid = go Set.empty []
  where
    go visited regions 
        | visited == allPositions = regions
        | otherwise = case findUnvisited visited of
            Just pos -> let newRegion = floodFill grid pos visited
                       in go (visited `Set.union` newRegion) (newRegion : regions)
            Nothing -> regions
      where
        allPositions = Set.fromList $ indices grid
        findUnvisited v = find (\p -> not $ p `Set.member` v) (indices grid)

floodFill :: Grid -> Pos -> Set.Set Pos -> Region
floodFill grid start _visited = go (Set.singleton start) (Set.singleton start)
  where
    targetChar = grid ! start
    go region frontier
        | Set.null frontier = region
        | otherwise = go newRegion newFrontier
      where
        neighbors pos = filter (isValid grid) $ adjacentPositions pos
        newPositions = Set.fromList $ concatMap
                        (filter isValidNeighbor . neighbors) (Set.toList frontier)
        isValidNeighbor p = not (p `Set.member` region) && 
                           grid ! p == targetChar
        newRegion = region `Set.union` newPositions
        newFrontier = newPositions

adjacentPositions :: Pos -> [Pos]
adjacentPositions (r, c) = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]

isValid :: Grid -> Pos -> Bool
isValid grid (r, c) = r >= minRow && r <= maxRow && 
                      c >= minCol && c <= maxCol
  where ((minRow, minCol), (maxRow, maxCol)) = bounds grid

calculateRegionPrice :: Region -> Int
calculateRegionPrice region = area * perimeter
  where
    area = Set.size region
    perimeter = sum [countExposedEdges pos | pos <- Set.toList region]
    countExposedEdges pos = length $ filter (not . (`Set.member` region)) 
                           (adjacentPositions pos)

countDistinctSides :: Grid -> Region -> Int
countDistinctSides grid region = 
    let char = grid ! (head $ Set.toList region)
        positions = Set.toList region
        byRow = Map.fromListWith Set.union [(r, Set.singleton c) | (r,c) <- positions]
        byCol = Map.fromListWith Set.union [(c, Set.singleton r) | (r,c) <- positions]
        
        -- Count breaks in each row and column
        rowSides = sum $ Map.mapWithKey countRowSides byRow
        colSides = sum $ Map.mapWithKey countColSides byCol
        
        totalSides = rowSides + colSides
        
        debugStr = unlines
            [ "Region " ++ [char] ++ ":"
            , "  Row sides: " ++ show rowSides
            , "  Col sides: " ++ show colSides
            , "  Total sides: " ++ show totalSides
            ]
    in trace debugStr totalSides
  where
    countRowSides _ coords = 
        let sorted = Set.toList coords
            groups = groupBy (\a b -> b - a == 1) sorted
        in length groups * 2  -- Left and right sides of each continuous segment

    countColSides _ coords = 
        let sorted = Set.toList coords
            groups = groupBy (\a b -> b - a == 1) sorted
        in length groups * 2  -- Top and bottom sides of each continuous segment

calculateRegionPricePart2 :: Grid -> Region -> Int
calculateRegionPricePart2 grid region = 
    let area = Set.size region
        sides = countDistinctSides grid region
        char = grid ! (head $ Set.toList region)
    in trace ("Region " ++ [char] ++ " price: area=" ++ show area ++ 
             " * sides=" ++ show sides ++ " = " ++ show (area * sides)) 
       (area * sides)

solveFuncPart2 :: String -> Int
solveFuncPart2 input = 
    let grid = parseInput input
        regions = findRegions grid
        prices = map (calculateRegionPricePart2 grid) regions
    in trace ("\nTotal regions: " ++ show (length regions) ++ 
             "\nPrices: " ++ show prices ++ 
             "\nTotal: " ++ show (sum prices)) $
       sum prices