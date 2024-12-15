module Day15.SolutionSpec (spec) where

import Test.Hspec
import Day15.Solution

spec :: Spec
spec = do
    describe "Day 15 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day15/simple.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day15/real.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

    describe "Day 15 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day15/simple.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day15/real.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result
