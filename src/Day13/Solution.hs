module Day13.Solution
    ( solve
    ,  solveFuncPart1
    ) where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day13/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    --putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Integer
solveFuncPart1 input = sum $ map solveMachine $ parseInput input
    where
        solveMachine (buttonA, buttonB, prize) = 
            case findCheapestSolution buttonA buttonB prize of
                Just (a, b) -> 3 * a + b
                Nothing -> 0

-- Helper types for clarity
type Point = (Integer, Integer)  -- Change from Int to Integer
type Machine = (Point, Point, Point)  -- (ButtonA, ButtonB, Prize)

parseInput :: String -> [Machine]
parseInput = map parseMachine . splitOn "\n\n"
    where
        parseMachine block = 
            let [buttonA, buttonB, prize] = lines block
                parsePoint str = (getX str, getY str)
                getX str = read $ case dropWhile (/= 'X') str of
                    'X':'=':rest -> takeWhile (/= ',') rest
                    'X':'+':rest -> takeWhile (/= ',') rest
                getY str = read $ case dropWhile (/= 'Y') str of
                    'Y':'=':rest -> rest
                    'Y':'+':rest -> rest
            in (parsePoint buttonA, parsePoint buttonB, parsePoint prize)



findCheapestSolution :: Point -> Point -> Point -> Maybe (Integer, Integer)
findCheapestSolution (ax, ay) (bx, by) (px, py) = 
    let solutions = [(a, b) | a <- [0..100], b <- [0..100],
                             ax * a + bx * b == px,
                             ay * a + by * b == py]
    in case solutions of
        [] -> Nothing
        xs -> Just $ minimumBy (comparing (\(a, b) -> 3 * a + b)) xs
