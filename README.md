# Advent of Code 2024

Haskell project for Advent of Code 2024.

Solutions per day:

- [Day 01](src/Day01/Solution.hs)
- [Day 02](src/Day02/Solution.hs)
- [Day 03](src/Day03/Solution.hs)
- [Day 04](src/Day04/Solution.hs)
- [Day 05](src/Day05/Solution.hs)
- [Day 06](src/Day06/Solution.hs)

Inputs are not stored in the repo.

## Project structure

This structure provides:

- A separate directory for each day's solution
- Automated test setup using HSpec
- A script to generate new day's files
- A proper Cabal project structure
- Input files organization

## Setup

To use this:

- Initialize a new project with these files
- For each new day, run ./setup-commands <day-number>
- Add the new module to the cabal file
- Write your solution in the corresponding src/DayXX/Solution.hs file
- Write tests in test/DayXX/SolutionSpec.hs
- Run tests with `cabal test`
- Run solutions with `cabal run`
