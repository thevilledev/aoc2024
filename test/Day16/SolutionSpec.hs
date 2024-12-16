module Day16.SolutionSpec (spec) where

import Test.Hspec
import Day16.Solution

spec :: Spec
spec = do
    describe "Day 16 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day16/simple.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day16/real.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

    describe "Day 16 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day16/simple.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day16/real.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result
