module Day15.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Debug.Trace (trace)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day15/small.txt"
    --putStrLn $ "\t* Part 1 (small): " ++ show (solveFuncPart1 input)
    input' <- readFile "inputs/Day15/large.txt"
    --putStrLn $ "\t* Part 1 (large): " ++ show (solveFuncPart1 input')
    input'' <- readFile "inputs/Day15/real.txt"
    --putStrLn $ "\t* Part 1 (real): " ++ show (solveFuncPart1 input'')

    --putStrLn $ "\t* Part 2 (small): " ++ show (solveFuncPart2 input)
    putStrLn $ "\t* Part 2 (large): " ++ show (solveFuncPart2 input')
    --putStrLn $ "\t* Part 2 (real): " ++ show (solveFuncPart2 input'')

solveFuncPart2 :: String -> Int
solveFuncPart2 input = calculateGPSSum2 $ simulateRobot2 wideGrid moves
  where
    (grid, moves) = parseInput input
    wideGrid = makeWideGrid grid

makeWideGrid :: Grid -> Grid
makeWideGrid = map expandRow
  where
    expandRow :: String -> String
    expandRow = concatMap expandChar

    expandChar :: Char -> String
    expandChar '#' = "##"
    expandChar 'O' = "[]"
    expandChar '@' = "@."
    expandChar '.' = ".."
    expandChar _ = ".."

isBox2 :: Grid -> Position -> Bool
isBox2 grid (r, c) = 
    isInBounds2 grid (r, c) && 
    isInBounds2 grid (r, c + 1) && 
    grid !! r !! c == '[' && 
    grid !! r !! (c + 1) == ']'

isInBounds2 :: Grid -> Position -> Bool
isInBounds2 grid (r, c) = 
    r >= 0 && r < length grid && c >= 0 && c < length (head grid)

simulateRobot2 :: Grid -> String -> Grid
simulateRobot2 grid [] = grid
simulateRobot2 grid (move:moves) = 
    case tryMove2 grid robotPos (getDirection move) of
        Just (newGrid, _) -> 
            trace ("\nAfter move " ++ [move] ++ ":\n" ++ unlines newGrid) $
            simulateRobot2 newGrid moves
        Nothing -> 
            trace ("\nFailed move " ++ [move] ++ ":\n" ++ unlines grid) $
            simulateRobot2 grid moves
  where
    robotPos = findRobot grid

tryMove2 :: Grid -> Position -> Position -> Maybe (Grid, Position)
tryMove2 grid (r, c) (dr, dc) =
    if canMove2 grid (r, c) (dr, dc)
    then Just (moveRobot2 grid (r, c) (dr, dc))
    else Nothing

canMove2 :: Grid -> Position -> Position -> Bool
canMove2 grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
    in isInBounds2 grid (newR, newC + 1) && 
       case (grid !! newR !! newC, grid !! newR !! (newC + 1)) of
           ('.', '.') -> True
           ('[', ']') -> canPushBox2 grid (newR, newC) (dr, dc)
           _ -> False

canPushBox2 :: Grid -> Position -> Position -> Bool
canPushBox2 grid (r, c) (dr, dc) =
    let nextR = r + dr
        nextC = c + dc
    in isInBounds2 grid (nextR, nextC + 1) &&
       case (grid !! nextR !! nextC, grid !! nextR !! (nextC + 1)) of
           ('.', '.') -> True
           ('[', ']') -> 
               -- Check if we can push both this box and any adjacent box
               isInBounds2 grid (nextR, nextC + 3) &&
               if grid !! nextR !! (nextC + 2) == '[' && grid !! nextR !! (nextC + 3) == ']'
               then canPushBox2 grid (nextR, nextC + 2) (dr, dc)
               else True
           _ -> False

moveRobot2 :: Grid -> Position -> Position -> (Grid, Position)
moveRobot2 grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
    in case (grid !! newR !! newC, grid !! newR !! (newC + 1)) of
        ('.', '.') -> 
            (updateGrid (updateGrid grid (r, c) '.') (newR, newC) '@', (newR, newC))
        ('[', ']') -> 
            if canPushBox2 grid (newR, newC) (dr, dc)
            then 
                let pushedGrid = pushBoxChain2 grid (newR, newC) (dr, dc)
                in (updateGrid (updateGrid pushedGrid (r, c) '.') (newR, newC) '@', (newR, newC))
            else (grid, (r, c))
        _ -> (grid, (r, c))

pushBoxChain2 :: Grid -> Position -> Position -> Grid
pushBoxChain2 grid (r, c) (dr, dc) =
    let nextR = r + dr
        nextC = c + dc
    in if not (isInBounds2 grid (nextR, nextC + 1)) 
       then grid
       else case (grid !! nextR !! nextC, grid !! nextR !! (nextC + 1)) of
           ('.', '.') -> 
               -- Move single box
               updateWideBox (updateWideBox grid (nextR, nextC) "[]") (r, c) ".."
           ('[', ']') -> 
               -- Push chain of boxes recursively
               let pushedGrid = pushBoxChain2 grid (nextR, nextC) (dr, dc)
               in if pushedGrid == grid
                  then grid  -- Could not push chain
                  else updateWideBox (updateWideBox pushedGrid (nextR, nextC) "[]") (r, c) ".."
           _ -> grid

updateWideBox :: Grid -> Position -> String -> Grid
updateWideBox grid (r, c) [ch1, ch2] =
    if isInBounds2 grid (r, c) && isInBounds2 grid (r, c + 1)
    then 
        let row = grid !! r
            newRow = take c row ++ [ch1, ch2] ++ drop (c + 2) row
        in take r grid ++ [newRow] ++ drop (r + 1) grid
    else grid
updateWideBox grid _ _ = grid

calculateGPSSum2 :: Grid -> Int
calculateGPSSum2 grid = sum [100 * r + (c `div` 2) | 
    r <- [0..length grid - 1],
    c <- [0..length (head grid) - 2],  -- Ensure we don't check last column
    let row = grid !! r,
    c + 1 < length row,
    row !! c == '[' && row !! (c + 1) == ']']

type Position = (Int, Int)
type Grid = [[Char]]

solveFuncPart1 :: String -> Int
solveFuncPart1 input = calculateGPSSum $ simulateRobot grid moves
  where
    (grid, moves) = parseInput input

parseInput :: String -> (Grid, String)
parseInput input = (gridLines, filter (`elem` "^v<>") moves)
  where
    (gridLines, movesLines) = break null $ lines input
    moves = concat $ drop 1 movesLines

findRobot :: Grid -> Position
findRobot grid = head [(r, c) | (r, row) <- zip [0..] grid,
                               (c, cell) <- zip [0..] row,
                               cell == '@']

simulateRobot :: Grid -> String -> Grid
simulateRobot grid [] = grid
simulateRobot grid (move:moves) = 
    case tryMove grid robotPos (getDirection move) of
        Just (newGrid, _) -> 
            --trace ("\nAfter move " ++ [move] ++ ":\n" ++ gridToString newGrid) $
            simulateRobot newGrid moves
        Nothing -> simulateRobot grid moves
  where
    robotPos = findRobot grid

--gridToString :: Grid -> String
--gridToString = unlines

getDirection :: Char -> Position
getDirection '^' = (-1, 0)
getDirection 'v' = (1, 0)
getDirection '<' = (0, -1)
getDirection '>' = (0, 1)
getDirection _ = (0, 0)

tryMove :: Grid -> Position -> Position -> Maybe (Grid, Position)
tryMove grid (r, c) (dr, dc) =
    if canMove grid (r, c) (dr, dc)
    then Just (moveRobot grid (r, c) (dr, dc))
    else Nothing

isInBounds :: Grid -> Position -> Bool
isInBounds grid (r, c) = 
    r >= 0 && r < length grid && c >= 0 && c < length (head grid)

canMove :: Grid -> Position -> Position -> Bool
canMove grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
    in isInBounds grid (newR, newC) && 
       case grid !! newR !! newC of
           '.' -> True
           'O' -> canPushBoxChain grid (newR, newC) (dr, dc)
           _ -> False

canPushBoxChain :: Grid -> Position -> Position -> Bool
canPushBoxChain grid (r, c) (dr, dc) =
    let nextR = r + dr
        nextC = c + dc
    in if not (isInBounds grid (nextR, nextC)) || grid !! nextR !! nextC == '#'
       then False
       else case grid !! nextR !! nextC of
           '.' -> True
           'O' -> canPushBoxChain grid (nextR, nextC) (dr, dc)
           _ -> False

moveRobot :: Grid -> Position -> Position -> (Grid, Position)
moveRobot grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
        targetCell = grid !! newR !! newC
    in case targetCell of
        '.' -> (updateGrid (updateGrid grid (r, c) '.') (newR, newC) '@', (newR, newC))
        'O' -> 
            let (finalGrid, finalPos) = pushBoxes grid (newR, newC) (dr, dc)
            in if finalPos == (newR, newC)
               then (grid, (r, c))  -- Box couldn't be pushed
               else (updateGrid (updateGrid finalGrid (r, c) '.') (newR, newC) '@', (newR, newC))
        _ -> (grid, (r, c))

pushBoxes :: Grid -> Position -> Position -> (Grid, Position)
pushBoxes grid (r, c) (dr, dc) =
    let newR = r + dr
        newC = c + dc
    in if not (isInBounds grid (newR, newC)) || grid !! newR !! newC == '#'
       then (grid, (r, c))
       else case grid !! newR !! newC of
           'O' -> 
               -- Try to push the next box in chain
               let (nextGrid, finalPos) = pushBoxes grid (newR, newC) (dr, dc)
               in if finalPos == (newR, newC)
                  then (grid, (r, c))  -- If next box couldn't move, this one can't either
                  else 
                      -- Successfully pushed next box, now move this one
                      let withNextBoxMoved = nextGrid
                          withCurrentBoxMoved = updateGrid withNextBoxMoved (newR, newC) 'O'
                          finalGrid = updateGrid withCurrentBoxMoved (r, c) '.'
                      in (finalGrid, (newR, newC))
           '.' -> 
               -- Simple case: move box to empty space
               let withBoxMoved = updateGrid grid (newR, newC) 'O'
                   finalGrid = updateGrid withBoxMoved (r, c) '.'
               in (finalGrid, (newR, newC))
           _ -> (grid, (r, c))

updateGrid :: Grid -> Position -> Char -> Grid
updateGrid grid (r, c) newChar =
    take r grid ++
    [take c (grid !! r) ++ [newChar] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid

calculateGPSSum :: Grid -> Int
calculateGPSSum grid = sum [100 * r + c | 
    (r, row) <- zip [0..] grid,
    (c, cell) <- zip [0..] row,
    cell == 'O']
