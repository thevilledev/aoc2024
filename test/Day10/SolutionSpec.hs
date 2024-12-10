module Day10.SolutionSpec (spec) where

import Test.Hspec
import Day10.Solution

spec :: Spec
spec = do
    describe "Day 10 - Part 1" $ do

        it "real input" $ do
            input <- readFile "inputs/Day10/real.txt"
            solveFuncPart1 input `shouldBe` 624  -- Replace with expected result

    describe "Day 10 - Part 2" $ do

        it "real input" $ do
            input <- readFile "inputs/Day10/real.txt"
            solveFuncPart2 input `shouldBe` 1483  -- Replace with expected result
