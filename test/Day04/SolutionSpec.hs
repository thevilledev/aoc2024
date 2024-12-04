module Day04.SolutionSpec (spec) where

import Test.Hspec
import Day04.Solution

spec :: Spec
spec = do
    describe "Day 04 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day04/simple.txt"
            solveFuncPart1 input `shouldBe` 18  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day04/real.txt"
            solveFuncPart1 input `shouldBe` 2591  -- Replace with expected result

    describe "Day 04 - Part 2" $ do
        it "solves part 1" $ do
            input <- readFile "inputs/Day04/simple2.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

        it "solves part 2" $ do
            input <- readFile "inputs/Day04/real.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result
