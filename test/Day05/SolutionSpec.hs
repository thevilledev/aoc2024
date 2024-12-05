module Day05.SolutionSpec (spec) where

import Test.Hspec
import Day05.Solution

spec :: Spec
spec = do
    describe "Day 05 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day05/simple.txt"
            solveFuncPart1 input `shouldBe` 143

        it "real input" $ do
            input <- readFile "inputs/Day05/real.txt"
            solveFuncPart1 input `shouldBe` 5091

    describe "Day 05 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day05/simple.txt"
            solveFuncPart2 input `shouldBe` 123

        it "real input" $ do
            input <- readFile "inputs/Day05/real.txt"
            solveFuncPart2 input `shouldBe` 4681
