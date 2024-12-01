module Day01.SolutionSpec (spec) where

import Test.Hspec
import Day01.Solution as Solution

spec :: Spec
spec = do
    describe "Day 01 - Part 1" $ do
        it "simple example" $ do
            input <- readFile "inputs/Day01/simple.txt"
            Solution.solveFuncPart1 input `shouldBe` 11

        it "real input" $ do
            input <- readFile "inputs/Day01/real.txt"
            Solution.solveFuncPart1 input `shouldBe` 2166959

    describe "Day 01 - Part 2" $ do
        it "simple example" $ do
            input <- readFile "inputs/Day01/simple.txt"
            Solution.solveFuncPart2 input `shouldBe` 31

        it "real input" $ do
            input <- readFile "inputs/Day01/real.txt"
            Solution.solveFuncPart2 input `shouldBe` 23741109
