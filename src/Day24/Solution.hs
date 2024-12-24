module Day24.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sortBy, sort, nub)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import Control.Monad (guard)
import Data.Bits (testBit)
import Debug.Trace (trace)

data GateOp = And | Or | Xor deriving (Show, Eq)
data Gate = Gate
    { input1 :: String
    , gateOp :: GateOp
    , input2 :: String
    , output :: String
    } deriving (Show, Eq)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day24/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    putStrLn "\t* Part 2: <run the python script>"
    --putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)

parseInitialValue :: String -> (String, Bool)
parseInitialValue line = 
    let [wire, val] = splitOn ":" line
    in (wire, read (filter isDigit val) == 1)

parseGate :: String -> Gate
parseGate line =
    case words line of
        [w1, op, w2, "->", out] -> Gate w1 (parseOp op) w2 out
        _ -> error $ "Invalid gate line: " ++ line
  where
    parseOp "AND" = And
    parseOp "OR" = Or
    parseOp "XOR" = Xor
    parseOp op = error $ "Unknown operation: " ++ op

-- Build maps for quick lookups
data CircuitMaps = CircuitMaps
    { gatesByOutput :: Map String Gate        -- output wire -> gate
    , gatesByInput :: Map String [Gate]       -- input wire -> gates using it
    }

buildCircuitMaps :: [Gate] -> CircuitMaps
buildCircuitMaps gates = CircuitMaps
    { gatesByOutput = Map.fromList [(output g, g) | g <- gates]
    , gatesByInput = foldr addGateInputs Map.empty gates
    }
  where
    addGateInputs gate = 
        Map.alter (Just . maybe [gate] (gate:)) (input1 gate) .
        Map.alter (Just . maybe [gate] (gate:)) (input2 gate)

-- Get bit number from a wire name (x00 -> 0, z45 -> 45)
getBitNum :: String -> Maybe Int
getBitNum wire = case reads (filter isDigit wire) :: [(Int, String)] of
    [(num, "")] -> Just num
    _ -> Nothing

-- Check if a gate represents XOR of x and y bits for a specific position
isXorOfXY :: CircuitMaps -> Int -> Gate -> Bool
isXorOfXY maps bitNum gate =
    let xBit = "x" ++ pad2 bitNum
        yBit = "y" ++ pad2 bitNum
        -- Check direct XOR of x and y
        directXor = gateOp gate == Xor &&
                   ((input1 gate == xBit && input2 gate == yBit) ||
                    (input1 gate == yBit && input2 gate == xBit))
        -- Check if inputs are intermediate gates that lead to x and y
        checkInput wire = case Map.lookup wire (gatesByOutput maps) of
            Just g -> isXorOfXY maps bitNum g
            Nothing -> False
    in directXor || 
       (gateOp gate == Xor && (checkInput (input1 gate) || checkInput (input2 gate)))
  where
    pad2 n = let s = show n in replicate (2 - length s) '0' ++ s

-- Check if a wire is a carry bit from the previous position
isCarryBit :: CircuitMaps -> Int -> String -> Bool
isCarryBit maps bitNum wire =
    take 1 wire == "c" && 
    case getBitNum wire of
        Just n -> n == bitNum - 1
        Nothing -> False

-- Check if a gate follows the sum bit pattern: (xN XOR yN) XOR carry(N-1)
isSumBitPattern :: CircuitMaps -> Int -> Gate -> Bool
isSumBitPattern maps bitNum gate =
    if gateOp gate /= Xor then False
    else
        let in1 = input1 gate
            in2 = input2 gate
            -- Check if one input is XOR of x,y and other is carry
            isPattern1 = isXorOfXY maps bitNum (fromMaybe gate $ Map.lookup in1 (gatesByOutput maps)) &&
                        isCarryBit maps bitNum in2
            isPattern2 = isXorOfXY maps bitNum (fromMaybe gate $ Map.lookup in2 (gatesByOutput maps)) &&
                        isCarryBit maps bitNum in1
            -- For bit 0, also accept direct XOR of x and y
            isPattern3 = bitNum == 0 && isXorOfXY maps bitNum gate
        in isPattern1 || isPattern2 || isPattern3

-- Check if a wire is part of the expected adder pattern for its bit position
isExpectedPattern :: CircuitMaps -> String -> Bool
isExpectedPattern maps wire = case getBitNum wire of
    Nothing -> True  -- Not a numbered wire, ignore
    Just bitNum -> 
        case Map.lookup wire (gatesByOutput maps) of
            Nothing -> True  -- No gate produces this wire
            Just g | take 1 wire == "z" -> isSumBitPattern maps bitNum g
            _ -> True  -- Not a z wire, ignore

-- Find suspect wires by checking structural patterns
findSuspectWires :: [Gate] -> [String]
findSuspectWires gates = 
    let maps = buildCircuitMaps gates
        zWires = filter (\w -> take 1 w == "z") $ Map.keys (gatesByOutput maps)
        suspects = filter (not . isExpectedPattern maps) zWires
        -- Debug information
        _ = trace ("Number of gates: " ++ show (length gates)) $
            trace ("Number of z-wires: " ++ show (length zWires)) $
            trace ("Checking sum bit patterns...") $
            trace (unlines $ map (\w -> 
                let g = Map.lookup w (gatesByOutput maps)
                    bitNum = fromMaybe (-1) $ getBitNum w
                in w ++ " (bit " ++ show bitNum ++ "): " ++ 
                   show g ++ " -> " ++ 
                   show (fmap (\g' -> isSumBitPattern maps bitNum g') g)) zWires) $
            ()
    in suspects

-- Original Part 1 solution remains unchanged
solveFuncPart1 :: String -> Int
solveFuncPart1 input = 
    let (initialSection:gateSection:_) = splitOn "\n\n" $ filter (/= '\r') input
        initialValues = Map.fromList $ map parseInitialValue $ filter (not . null) $ lines initialSection
        gates = map parseGate $ filter (not . null) $ lines gateSection
        finalWireMap = simulateGates gates initialValues
    in extractZWires finalWireMap

-- New Part 2 solution using structural analysis
solveFuncPart2 :: String -> String
solveFuncPart2 input = 
    let gateSection = last $ splitOn "\n\n" $ filter (/= '\r') input
        gates = map parseGate $ filter (not . null) $ lines gateSection
        suspectWires = sort $ findSuspectWires gates
    in case length suspectWires of
        8 -> concat $ zipWith (\w sep -> w ++ sep) suspectWires (replicate 7 "," ++ [""])
        n -> error $ "Expected 8 suspect wires (4 pairs), but found " ++ show n ++ 
                    "\nSuspect wires: " ++ show suspectWires

-- Helper functions from Part 1 that are still needed
simulateGates :: [Gate] -> Map String Bool -> Map String Bool
simulateGates gates initialMap = go initialMap gates
  where
    go currentMap [] = currentMap
    go currentMap remainingGates =
        let (ready, notReady) = partition (\g -> canEvaluate g currentMap) remainingGates
            newMap = foldr updateMap currentMap ready
        in if null ready 
           then currentMap
           else go newMap notReady

    canEvaluate gate m = all (`Map.member` m) [input1 gate, input2 gate]
    updateMap gate m = case evaluateGate gate m of
        Just val -> Map.insert (output gate) val m
        Nothing -> m

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = foldr select ([], []) xs
  where
    select x (ts, fs) | p x = (x:ts, fs)
                      | otherwise = (ts, x:fs)

evaluateGate :: Gate -> Map String Bool -> Maybe Bool
evaluateGate (Gate in1 op in2 _) wireMap = do
    v1 <- Map.lookup in1 wireMap
    v2 <- Map.lookup in2 wireMap
    return $ case op of
        And -> v1 && v2
        Or -> v1 || v2
        Xor -> v1 /= v2

extractZWires :: Map String Bool -> Int
extractZWires wireMap =
    let zWires = sortBy compareZWires $ filter isZWire $ Map.keys wireMap
        bits = map (\w -> fromMaybe False $ Map.lookup w wireMap) zWires
    in bitsToInt bits
  where
    isZWire (x:_) = x == 'z'
    isZWire _ = False
    compareZWires w1 w2 = compare (getNum w1) (getNum w2)
    getNum :: String -> Int
    getNum = read . filter isDigit
    bitsToInt = foldr (\b acc -> fromEnum b + 2 * acc) 0
