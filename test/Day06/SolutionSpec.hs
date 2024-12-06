module Day06.SolutionSpec (spec) where

import Test.Hspec
import Day06.Solution

spec :: Spec
spec = do
    describe "Day 06 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day06/simple.txt"
            solveFuncPart1 input `shouldBe` 41

        it "real input" $ do
            input <- readFile "inputs/Day06/real.txt"
            solveFuncPart1 input `shouldBe` 5212

    describe "Day 06 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day06/simple.txt"
            solveFuncPart2 input `shouldBe` 6

    --    it "real input" $ do
    --        input <- readFile "inputs/Day06/real.txt"
    --        solveFuncPart2 input `shouldBe` 1767
