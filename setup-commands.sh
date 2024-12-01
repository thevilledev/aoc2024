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
mkdir -p "inputs"

# Create solution file
cat > "src/Day$DAY/Solution.hs" << EOF
module Day$DAY.Solution
    ( solve
    ) where

solve :: IO ()
solve = do
    input <- readFile "inputs/day$DAY.txt"
    putStrLn \$ "Part 1: " ++ show (solvePart1 input)
    putStrLn \$ "Part 2: " ++ show (solvePart2 input)

solvePart1 :: String -> Int
solvePart1 _ = 0  -- TODO: Implement solution

solvePart2 :: String -> Int
solvePart2 _ = 0  -- TODO: Implement solution
EOF

# Create test file
cat > "test/Day$DAY/SolutionSpec.hs" << EOF
module Day$DAY.SolutionSpec (spec) where

import Test.Hspec
import Day$DAY.Solution

spec :: Spec
spec = do
    describe "Day $DAY" \$ do
        it "solves part 1" \$ do
            input <- readFile "inputs/day$DAY.txt"
            solvePart1 input \`shouldBe\` 0  -- Replace with expected result

        it "solves part 2" \$ do
            input <- readFile "inputs/day$DAY.txt"
            solvePart2 input \`shouldBe\` 0  -- Replace with expected result
EOF

# Create empty input file
touch "inputs/day$DAY.txt"

echo "Created files for Day $DAY"
echo "Don't forget to:"
echo "1. Add Day$DAY.Solution to exposed-modules in advent-of-code.cabal"
echo "2. Add Day$DAY.SolutionSpec to other-modules in advent-of-code.cabal"
echo "3. Import Day$DAY.Solution in app/Main.hs" 