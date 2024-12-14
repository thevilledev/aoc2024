module Day14.SolutionSpec (spec) where

import Test.Hspec
import Day14.Solution

spec :: Spec
spec = do
    describe "Day 14 - Part 1" $ do
    --    it "simple input" $ do
    --        input <- readFile "inputs/Day14/simple.txt"
    --        solveFuncPart1 input `shouldBe` 0  -- Replace with expected result

        it "real input" $ do
            input <- readFile "inputs/Day14/real.txt"
            solveFuncPart1 input `shouldBe` 224438715  -- Replace with expected result

    -- describe "Day 14 - Part 2" $ do
    --     it "simple input" $ do
    --         input <- readFile "inputs/Day14/simple.txt"
    --         solveFuncPart2 input `shouldBe` 0  -- Replace with expected result

    --     it "real input" $ do
    --         input <- readFile "inputs/Day14/real.txt"
    --         solveFuncPart2 input `shouldBe` 0  -- Replace with expected result
