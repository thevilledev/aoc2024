# Advent of Code 2024

Haskell project for Advent of Code 2024.

Solutions per day:

- [Day 01](src/Day01/Solution.hs)
- [Day 02](src/Day02/Solution.hs)
- [Day 03](src/Day03/Solution.hs)
- [Day 04](src/Day04/Solution.hs)
- [Day 05](src/Day05/Solution.hs)
- [Day 06](src/Day06/Solution.hs)
- [Day 07](src/Day07/Solution.hs)
- [Day 08](src/Day08/Solution.hs)
- [Day 09: Part 1](src/Day09/Solution.hs) + [Day 09: Part 2](src/Day09/part2.py)
- [Day 10](src/Day10/Solution.hs)
- [Day 11: Part 1](src/Day11/Solution.hs) + [Day 11: Part 2](src/Day11/part2.py)
- [Day 12: Part 1](src/Day12/Solution.hs) + [Day 12: Part 2](src/Day12/part2.py)
- [Day 13: Part 1](src/Day13/Solution.hs) + [Day 13: Part 2](src/Day13/part2.py)
- [Day 14: Part 1](src/Day14/Solution.hs) + [Day 14: Part 2](src/Day14/part2.py)
- [Day 15: Part 1](src/Day15/Solution.hs) + [Day 15: Part 2](src/Day15/part2.py)
- [Day 16: Part 1](src/Day16/Solution.hs) + [Day 16: Part 2](src/Day16/part2.py)
- [Day 17](src/Day17/Solution.hs)
- [Day 18](src/Day18/Solution.hs)
- [Day 19](src/Day19/Solution.hs)
- [Day 20](src/Day19/solution.py)

Inputs are not stored in the repo.

Some Python solutions included due to lack of time to implement them properly in Haskell.

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
