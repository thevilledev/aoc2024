module Day03.SolutionSpec (spec) where

import Test.Hspec
import Day03.Solution

spec :: Spec
spec = do
    describe "Day 03 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day03/simple.txt"
            solveFuncPart1 input `shouldBe` 161

        it "real input" $ do
            input <- readFile "inputs/Day03/real.txt"
            solveFuncPart1 input `shouldBe` 165225049

    describe "Day 03 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day03/simple2.txt"
            solveFuncPart2 input `shouldBe` 48

        it "real input" $ do
            input <- readFile "inputs/Day03/real.txt"
            solveFuncPart2 input `shouldBe` 108830766
