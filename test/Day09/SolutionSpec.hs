module Day09.SolutionSpec (spec) where

import Test.Hspec
import Day09.Solution

spec :: Spec
spec = do
    describe "Day 09 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day09/simple.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day09/real.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

    describe "Day 09 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day09/simple.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day09/real.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result
