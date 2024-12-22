module Day22.SolutionSpec (spec) where

import Test.Hspec
import Day22.Solution

spec :: Spec
spec = do
    describe "Day 22 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day22/simple.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day22/real.txt"
            solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

    describe "Day 22 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day22/simple.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day22/real.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result
