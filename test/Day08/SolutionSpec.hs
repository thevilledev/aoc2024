module Day08.SolutionSpec (spec) where

import Test.Hspec
import Day08.Solution

spec :: Spec
spec = do
    describe "Day 08 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day08/simple.txt"
            solveFuncPart1 input `shouldBe` 14

        it "real input" $ do
            input <- readFile "inputs/Day08/real.txt"
            solveFuncPart1 input `shouldBe` 376

    describe "Day 08 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day08/simple.txt"
            solveFuncPart2 input `shouldBe` 34

        it "real input" $ do
            input <- readFile "inputs/Day08/real.txt"
            solveFuncPart2 input `shouldBe` 1352
