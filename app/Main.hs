module Main where

import qualified Day01.Solution as Day01
import qualified Day02.Solution as Day02
-- Import more days as you add them

main :: IO ()
main = do
    putStrLn "Advent of Code 2024 Solutions"
    putStrLn "* Day 1:"
    Day01.solve 
    putStrLn "* Day 2:"
    Day02.solve 