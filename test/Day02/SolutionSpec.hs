module Day02.SolutionSpec (spec) where

import Test.Hspec
import Day02.Solution

spec :: Spec
spec = do
    describe "Day 02 - Part 1" $ do
        it "simple example" $ do
            input <- readFile "inputs/Day02/simple.txt"
            solveFuncPart1 input `shouldBe` 2
        it "real input" $ do
             input <- readFile "inputs/Day02/real.txt"
             solveFuncPart1 input `shouldBe` 564

    describe "Day 02 - Part 2" $ do
        it "simple example" $ do
            input <- readFile "inputs/Day02/simple.txt"
            solveFuncPart2 input `shouldBe` 4

        it "real input" $ do
             input <- readFile "inputs/Day02/real.txt"
             solveFuncPart2 input `shouldBe` 604
