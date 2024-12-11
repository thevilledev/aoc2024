module Day11.SolutionSpec (spec) where

import Test.Hspec
import Day11.Solution

spec :: Spec
spec = do
    describe "Day 11 - Part 1" $ do
        --it "simple input" $ do
        --    input <- readFile "inputs/Day11/simple.txt"
        --    solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day11/real.txt"
            solveFuncPart1 input `shouldBe` 189547  -- Replace with expected result

    describe "Day 11 - Part 2" $ do
       -- it "simple input" $ do
       --    input <- readFile "inputs/Day11/simple.txt"
       --    solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day11/real.txt"
            solveFuncPart2 input `shouldBe` 224577979481346  -- Replace with expected result
