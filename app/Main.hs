module Main where

import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
import qualified Day03.Solution as Day03
import qualified Day04.Solution as Day04
import qualified Day05.Solution as Day05
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
