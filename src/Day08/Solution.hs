module Day08.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

import Data.List (nub, sortOn, groupBy, tails)
-- import Data.IORef
-- import System.IO.Unsafe
-- import Debug.Trace (trace)
-- import Data.Char (gcd)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day08/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 input = length $ nub antinodes
  where
    grid = parseGrid input
    frequencies = findFrequencies grid
    bounds = getBounds input
    antinodes = concatMap (filter (inBounds bounds) . findAntinodesForFreq grid) frequencies

parseGrid :: String -> [(Char, (Int, Int))]
parseGrid input = [(c, (row, col)) | 
    (row, line) <- zip [0..] (lines input),
    (col, c) <- zip [0..] line,
    c /= '.']

findFrequencies :: [(Char, (Int, Int))] -> [[((Int, Int), Char)]]
findFrequencies grid = groupBy (\a b -> snd a == snd b) . 
    sortOn snd $ [(_pos, _freq) | (_freq, _pos) <- grid]

findAntinodesForFreq :: [(Char, (Int, Int))] -> [((Int, Int), Char)] -> [(Int, Int)]
findAntinodesForFreq _grid antennas = 
    [antinode | 
    (((r1, c1), _), ((r2, c2), _)) <- pairs antennas,
    antinode <- calculateAntinodes (r1, c1) (r2, c2)]

calculateAntinodes :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
calculateAntinodes (r1, c1) (r2, c2) = 
    let dr = r2 - r1
        dc = c2 - c1
        gcd' = gcd dr dc
        unitDr = dr `div` gcd'
        unitDc = dc `div` gcd'
        -- Generate points in both directions
        forwards = [(r2 + k*unitDr, c2 + k*unitDc) | k <- [1..gcd']]
        backwards = [(r1 - k*unitDr, c1 - k*unitDc) | k <- [1..gcd']]
    in forwards ++ backwards

getBounds :: String -> ((Int, Int), (Int, Int))
getBounds input = 
    let ls = lines input
        rows = length ls
        cols = length (head ls)
    in ((0, 0), (rows - 1, cols - 1))

inBounds :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inBounds ((minR, minC), (maxR, maxC)) (r, c) = 
    r >= minR && r <= maxR && c >= minC && c <= maxC

solveFuncPart2 :: String -> Int
solveFuncPart2 input = 
    let grid = parseGrid input
        frequencies = findFrequencies grid
        bounds = getBounds input
        antinodes = concatMap (findCollinearPoints bounds) frequencies
    in length $ nub antinodes

findCollinearPoints :: ((Int, Int), (Int, Int)) -> [((Int, Int), Char)] -> [(Int, Int)]
findCollinearPoints bounds antennas = 
    let positions = map fst antennas
        -- Check if point lies on line between two other points
        isOnLine (x, y) (x1, y1) (x2, y2) =
            let crossProduct = (y2 - y1) * (x - x1) - (x2 - x1) * (y - y1)
            in crossProduct == 0

        -- Get all possible points within bounds
        allPossiblePoints = [(r,c) | r <- [minR..maxR], c <- [minC..maxC]]
            where ((minR,minC),(maxR,maxC)) = bounds

        -- Check if a point is an antinode (lies on any line between two positions)
        isAntinode point = any (\p1 -> any (\p2 -> 
            p1 /= p2 && point /= p1 && point /= p2 && isOnLine point p1 p2
            ) positions) positions

        -- Get points between each pair
        pairPoints = if length positions >= 2
                    then concatMap (uncurry getAllCollinearPoints) 
                         [(p1, p2) | p1 <- positions, p2 <- positions, p1 < p2]
                    else positions

        -- Get additional points that lie on any line
        additionalPoints = filter isAntinode allPossiblePoints

        allPoints = nub $ pairPoints ++ additionalPoints
    in filter (inBounds bounds) allPoints

getAllCollinearPoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAllCollinearPoints (r1, c1) (r2, c2) = 
    let dr = r2 - r1
        dc = c2 - c1
        gcd' = gcd (abs dr) (abs dc)
        unitDr = dr `div` gcd'
        unitDc = dc `div` gcd'
        -- Get ALL points along the line damn it
        minK = -(2 * max (abs dr) (abs dc))  -- Extended range
        maxK = 2 * max (abs dr) (abs dc)   -- Extended range
        points = [(r1 + k*unitDr, c1 + k*unitDc) |
                 k <- [minK..maxK]]
    in points

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (x:ys) <- tails xs, y <- ys]
