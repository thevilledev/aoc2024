module Day12.SolutionSpec (spec) where

import Test.Hspec
import Day12.Solution

spec :: Spec
spec = do
    describe "Day 12 - Part 1" $ do
        -- it "simple input" $ do
        --     input <- readFile "inputs/Day12/simple.txt"
        --     solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day12/real.txt"
            solveFuncPart1 input `shouldBe` 1449902  -- Replace with expected result

    describe "Day 12 - Part 2" $ do
        -- it "simple input" $ do
        --     input <- readFile "inputs/Day12/simple.txt"
        --     solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day12/real.txt"
            solveFuncPart2 input `shouldBe` 0  -- Replace with expected result
