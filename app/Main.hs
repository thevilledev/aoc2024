module Main where

import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
import qualified Day03.Solution as Day03
import qualified Day04.Solution as Day04
import qualified Day05.Solution as Day05
-- import qualified Day06.Solution as Day06
--import qualified Day07.Solution as Day07
import qualified Day08.Solution as Day08
import qualified Day09.Solution as Day09
import qualified Day10.Solution as Day10
import qualified Day11.Solution as Day11
import qualified Day12.Solution as Day12
import qualified Day13.Solution as Day13
import qualified Day14.Solution as Day14
-- Import more days as you add them

main :: IO ()
main = do
    putStrLn "Advent of Code 2024 Solutions"
    putStrLn "* Day 1:"
    Day01.solve 
    putStrLn "* Day 2:"
    Day02.solve
    putStrLn "* Day 3:"
    Day03.solve
    putStrLn "* Day 4:"
    Day04.solve
    putStrLn "* Day 5:"
    Day05.solve
    putStrLn "* Day 6:"
    -- commented out because it takes a long time to run
    -- Day06.solve
    putStrLn "* Day 7:"
    -- Day07.solve
    putStrLn "* Day 8:"
    Day08.solve
    putStrLn "* Day 9:"
    Day09.solve
    putStrLn "* Day 10:"
    Day10.solve
    putStrLn "* Day 11:"
    Day11.solve
    putStrLn "* Day 12:"
    Day12.solve
    putStrLn "* Day 13:"
    Day13.solve
    putStrLn "* Day 14:"
    Day14.solve
