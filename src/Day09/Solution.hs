module Day09.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import Data.List (groupBy)
import Debug.Trace (trace)

solve :: IO ()
solve = do
    --input <- readFile "inputs/Day09/simple.txt"
    --putStrLn $ "\t* Part simple: " ++ show (solveFuncPart1 input)
    --input' <- readFile "inputs/Day09/simple.txt"
    putStrLn "\t* Part 1: <takes too long>"
    putStrLn "\t* Part 2: <run the python script>"
    --putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input')
    --putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input')

solveFuncPart1 :: String -> Int
solveFuncPart1 = calculateChecksum . compactDisk . parseInput . head . lines

-- Parse the input string into pairs of file lengths and free space lengths
parseInput :: String -> [(Int, Bool)]
parseInput s = concat $ zipWith makeBlock [0..] $ getPairs $ map (read . pure) s
  where
    getPairs [] = []
    getPairs (x:y:rest) = (x,y) : getPairs rest
    getPairs [x] = [(x,0)]  -- Handle last single number as file with no space
    
    makeBlock fileId (fileLen, spaceLen) = 
        replicate fileLen (fileId, True) ++    -- File blocks
        replicate spaceLen (-1, False)         -- Space blocks

-- Compact the disk by processing files from right to left
compactDisk :: [(Int, Bool)] -> [(Int, Bool)]
compactDisk disk = foldr moveBlock disk filePositions
  where
    filePositions = [(pos, fId) | (pos, (fId, isFile)) <- zip [0..] disk, isFile]
    
    moveBlock (pos, fileId) d =
        let targetPos = findFirstSpace d pos
            file = (fileId, True)
        in if targetPos < pos
           then take targetPos d ++
                [file] ++
                take (pos - targetPos - 1) (drop (targetPos + 1) d) ++
                [(-1, False)] ++
                drop (pos + 1) d
           else d

findFirstSpace :: [(Int, Bool)] -> Int -> Int
findFirstSpace disk pos =
    case [i | (i, (_, isFile)) <- zip [0..] (take pos disk), not isFile] of
        [] -> pos
        xs -> head xs  -- Take the leftmost space

calculateChecksum :: [(Int, Bool)] -> Int
calculateChecksum disk = sum [pos * fileId | (pos, (fileId, True)) <- zip [0..] disk]

solveFuncPart2 :: String -> Int
solveFuncPart2 = calculateChecksum . compactDiskWholeFiles . parseInput . head . lines

compactDiskWholeFiles :: [(Int, Bool)] -> [(Int, Bool)]
compactDiskWholeFiles disk = 
    trace ("Initial disk state: " ++ showDisk disk) $
    foldl processFile disk [maxId,maxId-1..0]
  where
    maxId = maximum [fId | (fId, True) <- disk]
    
    processFile :: [(Int, Bool)] -> Int -> [(Int, Bool)]
    processFile d fileId = 
        trace ("\nProcessing file ID: " ++ show fileId) $
        case findFile d fileId of
            Nothing -> trace "  File not found" d
            Just (startPos, len) -> 
                let spaces = findAllContiguousSpaces d len
                    validSpaces = [pos | pos <- spaces, pos < startPos]
                in trace ("  File found at pos " ++ show startPos ++ 
                         " with length " ++ show len ++
                         "\n  Available spaces: " ++ show spaces ++
                         "\n  Valid spaces: " ++ show validSpaces) $
                case validSpaces of
                    [] -> trace "  No valid space found, keeping file in place" d
                    _ -> let targetPos = minimum validSpaces
                         in moveFileBlock d fileId startPos targetPos len

-- New helper function to find all spaces of sufficient size
findAllContiguousSpaces :: [(Int, Bool)] -> Int -> [Int]
findAllContiguousSpaces disk neededLen = 
    let spaces = findSpaces disk 0
    in trace ("  Finding spaces of size " ++ show neededLen ++
             "\n  All spaces found: " ++ show spaces) $
    [pos | (pos, len) <- spaces, len >= neededLen]
  where
    findSpaces :: [(Int, Bool)] -> Int -> [(Int, Int)]
    findSpaces [] _ = []
    findSpaces xs pos =
        let (nonSpaces, rest1) = span snd xs
            (spaces, rest2) = break snd rest1
            newPos = pos + length nonSpaces
        in if null spaces 
           then []
           else (newPos, length spaces) : 
                findSpaces rest2 (newPos + length spaces)

-- Updated moveFile function to handle the movement more precisely
moveFileBlock :: [(Int, Bool)] -> Int -> Int -> Int -> Int -> [(Int, Bool)]
moveFileBlock disk fileId from to len =
    let beforeTarget = take to disk                                    -- Before the target position
        betweenPart = take (from - to - len) (drop to disk)         -- Between target and original, excluding file space
        afterFile = drop (from + len) disk                            -- After the file
        fileBlocks = replicate len (fileId, True)                     -- New file blocks
        spaceBlocks = replicate len (-1, False)                       -- Space blocks to replace moved file
        
        -- Calculate result
        result = beforeTarget ++                -- Part before target
                fileBlocks ++                   -- The moved file
                betweenPart ++                  -- Part between target and original (excluding file space)
                spaceBlocks ++                  -- Spaces where file was
                afterFile                       -- Rest of disk
                
    in trace ("  Moving blocks:" ++
             "\n    Before target (" ++ show (length beforeTarget) ++ "): " ++ showDisk beforeTarget ++
             "\n    File blocks (" ++ show (length fileBlocks) ++ "): " ++ showDisk fileBlocks ++
             "\n    Between part (" ++ show (length betweenPart) ++ "): " ++ showDisk betweenPart ++
             "\n    Space blocks (" ++ show (length spaceBlocks) ++ "): " ++ showDisk spaceBlocks ++
             "\n    After file (" ++ show (length afterFile) ++ "): " ++ showDisk afterFile ++
             "\n    From: " ++ show from ++
             "\n    To: " ++ show to ++
             "\n    Len: " ++ show len) $
    if length result == length disk
    then result
    else error $ "Parts sum to wrong length: " ++ show (length result) ++ 
                "\nBefore: " ++ show (length beforeTarget) ++
                "\nFile: " ++ show (length fileBlocks) ++
                "\nBetween: " ++ show (length betweenPart) ++
                "\nSpace: " ++ show (length spaceBlocks) ++
                "\nAfter: " ++ show (length afterFile) ++
                "\nDisk: " ++ show (length disk)

findFile :: [(Int, Bool)] -> Int -> Maybe (Int, Int)
findFile disk fileId = 
    let groups = groupBy sameFile disk
        positions = zip [0..] groups
        fileGroups = [(pos, length g) | (pos, g) <- positions, 
                                      not (null g), 
                                      let (fId, isFile) = head g,
                                      isFile && fId == fileId]
    in case fileGroups of
        [] -> Nothing
        (x:_) -> Just x
  where
    sameFile (id1, isFile1) (id2, isFile2) = 
        isFile1 && isFile2 && id1 == id2

-- Helper function to visualize disk state
showDisk :: [(Int, Bool)] -> String
showDisk = concatMap showBlock
  where
    showBlock (fileId, True) = show fileId
    showBlock (_, False) = "."
