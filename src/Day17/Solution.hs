module Day17.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Data.List (intercalate)
import Data.Bits (xor)

data ComputerState = ComputerState
    { registerA :: Integer
    , registerB :: Integer
    , registerC :: Integer
    , instructionPtr :: Int
    , program :: [Int]
    , output :: [Int]
    } deriving (Show)

-- Parse register value from a line like "Register A: 729"
parseRegisterValue :: String -> Integer
parseRegisterValue line = 
    read $ filter (`elem` "0123456789") $ dropWhile (/= ':') line

-- Parse input string into initial state and program
parseInput :: String -> (Integer, Integer, Integer, [Int])
parseInput input = 
    let ls = filter (not . null) $ lines input
        regA = parseRegisterValue (ls !! 0)  -- First line has Register A
        regB = parseRegisterValue (ls !! 1)  -- Second line has Register B
        regC = parseRegisterValue (ls !! 2)  -- Third line has Register C
        programLine = last ls  -- Last line has the program
        cleanLine = dropWhile (/= ':') programLine
        numbers = filter (not . null) $ 
                 words $ 
                 map (\c -> if c == ',' then ' ' else c) $
                 filter (\c -> c `elem` "0123456789, ") cleanLine
    in (regA, regB, regC, map read numbers)

-- Initialize computer state with program and register values
initState :: (Integer, Integer, Integer, [Int]) -> ComputerState
initState (regA, regB, regC, prog) = ComputerState
    { registerA = regA
    , registerB = regB
    , registerC = regC
    , instructionPtr = 0
    , program = prog
    , output = []
    }

-- Resolve combo operand based on rules
resolveComboOperand :: ComputerState -> Int -> Integer
resolveComboOperand state operand = case operand of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> registerA state
    5 -> registerB state
    6 -> registerC state
    7 -> error "Invalid combo operand 7"

-- Get division operand (2^n where n is the combo operand value)
getDivOperand :: ComputerState -> Int -> Integer
getDivOperand state n = 
    let power = resolveComboOperand state n
    in 2 ^ power

-- Execute a single instruction
executeInstruction :: ComputerState -> ComputerState
executeInstruction state =
    if instructionPtr state >= length (program state)
        then state  -- Program has halted
        else
            let 
                opcode = program state !! instructionPtr state
                operand = if instructionPtr state + 1 < length (program state)
                            then program state !! (instructionPtr state + 1)
                            else error "Program reads past end"
                comboValue = resolveComboOperand state operand
                divValue = getDivOperand state operand
            in case opcode of
                    0 -> state  -- ADV: Division result to A
                        { registerA = registerA state `div` divValue
                        , instructionPtr = instructionPtr state + 2
                        }
                    1 -> state  -- BXL: XOR with literal to B (uses literal operand)
                        { registerB = registerB state `xor` fromIntegral operand
                        , instructionPtr = instructionPtr state + 2
                        }
                    2 -> state  -- BST: Store combo mod 8 to B
                        { registerB = comboValue `mod` 8
                        , instructionPtr = instructionPtr state + 2
                        }
                    3 -> state  -- JNZ: Jump if A non-zero (uses literal operand)
                        { instructionPtr = if registerA state /= 0
                                         then operand
                                         else instructionPtr state + 2
                        }
                    4 -> state  -- BXC: XOR B with C to B (ignores operand)
                        { registerB = registerB state `xor` registerC state
                        , instructionPtr = instructionPtr state + 2
                        }
                    5 -> state  -- OUT: Output combo mod 8
                        { output = output state ++ [fromIntegral (comboValue `mod` 8)]
                        , instructionPtr = instructionPtr state + 2
                        }
                    6 -> state  -- BDV: Division result to B
                        { registerB = registerA state `div` divValue  -- Note: uses A as numerator
                        , instructionPtr = instructionPtr state + 2
                        }
                    7 -> state  -- CDV: Division result to C
                        { registerC = registerA state `div` divValue  -- Note: uses A as numerator
                        , instructionPtr = instructionPtr state + 2
                        }
                    _ -> error "Invalid opcode"

-- Run program until completion
runProgram :: ComputerState -> ComputerState
runProgram state =
    if instructionPtr state >= length (program state)
        then state
        else runProgram (executeInstruction state)

-- Format output as comma-separated string
formatOutput :: [Int] -> String
formatOutput [] = ""
formatOutput [x] = show x
formatOutput (x:xs) = show x ++ "," ++ formatOutput xs

solve :: IO ()
solve = do
    input <- readFile "inputs/Day17/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

solveFuncPart1 :: String -> String
solveFuncPart1 input = 
    let initialState = initState $ parseInput input
        finalState = runProgram initialState
    in formatOutput (output finalState)

-- Part 2: Find minimum A that makes program output itself

-- Run program until first output
runUntilOutput :: ComputerState -> [Int]
runUntilOutput state =
    if instructionPtr state >= length (program state)
        then output state
        else let newState = executeInstruction state
             in if length (output newState) > length (output state)
                then output newState
                else runUntilOutput newState

-- Try to find A recursively, building it from right to left
reverseSolve :: [Int] -> Int -> Integer -> Maybe Integer
reverseSolve program position register =
    if position < 0
        then Just register
        else findMatch [0..7]
    where
        findMatch [] = Nothing
        findMatch (d:ds) = 
            let newA = register * 8 + fromIntegral d
                state = initState (newA, 0, 0, program)
                firstOutput = runUntilOutput state
            in if not (null firstOutput) && head firstOutput == program !! position
                then case reverseSolve program (position-1) newA of
                    Just result -> Just result
                    Nothing -> findMatch ds
                else findMatch ds

solveFuncPart2 :: String -> Integer
solveFuncPart2 input = 
    let (_, _, _, program) = parseInput input
        result = reverseSolve program (length program - 1) 0
    in case result of
        Just a -> a
        Nothing -> error "No solution found"
