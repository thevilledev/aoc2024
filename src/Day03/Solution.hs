-- | Module for solving Day 03 challenges.
module Day03.Solution
    ( solve
    , solveFuncPart1
    , solveFuncPart2
    ) where

import Data.Char (isDigit)

-- | Main function to solve both parts of the challenge.
solve :: IO ()
solve = do
    input <- readFile "inputs/Day03/real.txt"
    let part1Result = solveFuncPart1 input
    putStrLn $ "\t* Part 1: " ++ show part1Result
    input2 <- readFile "inputs/Day03/real.txt"
    let part2Result = solveFuncPart2 input2
    putStrLn $ "\t* Part 2: " ++ show part2Result

-- | Represents the state of the parser.
data ParserState 
    = Initial
    | Ignore    -- New state for ignore mode
    | ReadD     -- New state for both do and don't
    | ReadO
    | ReadN     -- For don't
    | ReadApos  -- For don't
    | ReadT     -- For don't
    | ReadM
    | ReadU
    | ReadL
    | ReadLeftParen
    | ReadFirstNum String
    | ReadComma String
    | ReadSecondNum String Int
    deriving (Show, Eq)

-- | Represents a parsed multiplication expression or an invalid result.
data ParseResult = MulExpr Int Int | Invalid
    deriving (Show)

-- | Parses a character based on the current parser state.
parseChar :: Bool -> ParserState -> Char -> ParserState
parseChar useDoOperators state c = case state of
    -- Only process do/don't operators if feature flag is enabled
    _ | not useDoOperators -> case state of
        Initial | c == 'm' -> ReadM
        ReadM | c == 'u' -> ReadU
        ReadU | c == 'l' -> ReadL
        ReadL | c == '(' -> ReadLeftParen
        ReadLeftParen | isDigit c -> ReadFirstNum [c]
        ReadFirstNum nums | isDigit c -> ReadFirstNum (c:nums)
                         | c == ',' -> ReadComma nums
        ReadComma nums | isDigit c -> ReadSecondNum [c] (read $ reverse nums)
        ReadSecondNum nums firstNum | isDigit c -> ReadSecondNum (c:nums) firstNum
        _ -> Initial

    -- Group "don't" related states
    Ignore | c == 'd' -> ReadD
    Ignore -> Ignore
    ReadN | c == '\'' -> ReadApos
    ReadApos | c == 't' -> ReadT
    ReadT | c == '(' -> Ignore

    -- Group "do/initial" related states
    Initial | c == 'd' -> ReadD
            | c == 'm' -> ReadM
    ReadD | c == 'o' -> ReadO
    ReadO | c == 'n' -> ReadN
          | c == '(' -> Initial

    -- Group multiplication related states
    ReadM | c == 'u' -> ReadU
    ReadU | c == 'l' -> ReadL
    ReadL | c == '(' -> ReadLeftParen

    -- Group number parsing states
    ReadLeftParen | isDigit c -> ReadFirstNum [c]
    ReadFirstNum nums | isDigit c -> ReadFirstNum (c:nums)
                     | c == ',' -> ReadComma nums
    ReadComma nums | isDigit c -> ReadSecondNum [c] (read $ reverse nums)
    ReadSecondNum nums firstNum | isDigit c -> ReadSecondNum (c:nums) firstNum

    -- Fallback case
    _ -> Initial

-- | Parses multiplication expressions from a string.
parseMulExpressions :: Bool -> String -> [ParseResult]
parseMulExpressions useDoOperators = go Initial []
  where
    go :: ParserState -> [ParseResult] -> String -> [ParseResult]
    go _ results [] = reverse results
    go state results (c:rest) = case state of
        Ignore -> go (parseChar useDoOperators Ignore c) results rest
        ReadO | c == '(' -> go Initial results rest    -- "do()" deactivates ignore mode
        ReadSecondNum nums firstNum | c == ')' ->
            go Initial (MulExpr firstNum (read $ reverse nums) : results) rest
        _ -> go (parseChar useDoOperators state c) results rest

-- | Solves part 1 of the challenge by summing the results of parsed expressions.
solveFuncPart1 :: String -> Int
solveFuncPart1 input = sum [x * y | MulExpr x y <- parseMulExpressions False input]

-- | Updated solveFuncPart2 that uses the new parser
solveFuncPart2 :: String -> Int
solveFuncPart2 input = sum [x * y | MulExpr x y <- parseMulExpressions True input]
