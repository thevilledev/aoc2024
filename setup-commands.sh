#!/bin/bash

# Usage: ./setup-commands <day-number>
# Example: ./setup-commands 05

if [ -z "$1" ]; then
    echo "Please provide a day number (e.g., ./setup-commands 05)"
    exit 1
fi

# Pad day number with leading zero if necessary
DAY=$(printf "%02d" "$1")

# Create directories
mkdir -p "src/Day$DAY"
mkdir -p "test/Day$DAY"
mkdir -p "inputs/Day$DAY"

# Create solution file
cat > "src/Day$DAY/Solution.hs" << EOF
module Day$DAY.Solution
    ( solve
      solveFuncPart1
      solveFuncPart2
    ) where

solve :: IO ()
solve = do
    input <- readFile "inputs/Day$DAY/real.txt"
    putStrLn \$ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn \$ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> Int
solveFuncPart1 _ = 0  -- TODO: Implement solution

solveFuncPart2 :: String -> Int
solveFuncPart2 _ = 0  -- TODO: Implement solution
EOF

# Create test file
cat > "test/Day$DAY/SolutionSpec.hs" << EOF
module Day$DAY.SolutionSpec (spec) where

import Test.Hspec
import Day$DAY.Solution

spec :: Spec
spec = do
    describe "Day $DAY - Part 1" \$ do
        it "simple input" \$ do
            input <- readFile "inputs/Day$DAY/simple.txt"
            solveFuncPart1 input \`shouldBe\` 0  -- Replace with expected result

        it "real input" \$ do
            input <- readFile "inputs/Day$DAY/real.txt"
            solveFuncPart1 input \`shouldBe\` 0  -- Replace with expected result

    describe "Day $DAY - Part 2" \$ do
        it "simple input" \$ do
            input <- readFile "inputs/Day$DAY/simple.txt"
            solveFuncPart2 input \`shouldBe\` 0  -- Replace with expected result

        it "real input" \$ do
            input <- readFile "inputs/Day$DAY/real.txt"
            solveFuncPart2 input \`shouldBe\` 0  -- Replace with expected result
EOF

# Create empty input files
touch "inputs/Day$DAY/simple.txt"
touch "inputs/Day$DAY/real.txt"

echo "Created files for Day $DAY"
echo "Don't forget to:"
echo "1. Add Day$DAY.Solution to exposed-modules in advent-of-code.cabal"
echo "2. Add Day$DAY.SolutionSpec to other-modules in advent-of-code.cabal"
echo "3. Import Day$DAY.Solution in app/Main.hs" 
