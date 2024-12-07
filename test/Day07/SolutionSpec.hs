module Day07.SolutionSpec (spec) where

import Test.Hspec
import Day07.Solution

spec :: Spec
spec = do
    describe "Day 07 - Part 1" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day07/simple.txt"
            solveFuncPart1 input `shouldBe` 3749
        it "real input" $ do
            input <- readFile "inputs/Day07/real.txt"
            solveFuncPart1 input `shouldBe` 1430271835320


    describe "Day 07 - Part 2" $ do
        it "simple input" $ do
            input <- readFile "inputs/Day07/simple.txt"
            solveFuncPart2 input `shouldBe` 11387

        -- commented out because it takes a long time to run
        -- it "real input" $ do
        --     input <- readFile "inputs/Day07/real.txt"
        --     solveFuncPart2 input `shouldBe` 456565678667482

