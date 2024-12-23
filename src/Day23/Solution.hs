module Day23.Solution
    ( solve
    ,  solveFuncPart1
    ,  solveFuncPart2
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, intercalate, maximumBy)
import Data.Ord (comparing)
import Data.List.Split (splitOn)

type Node = String
type Graph = Map.Map Node (Set.Set Node)
type Triplet = (Node, Node, Node)

solve :: IO ()
solve = do
    input <- readFile "inputs/Day23/real.txt"
    putStrLn $ "\t* Part 1: " ++ show (solveFuncPart1 input)
    --putStrLn $ "\t* Part 2: " ++ show (solveFuncPart2 input)
    putStrLn "\t* Part 2: <uncomment to run>"

buildGraph :: [String] -> Graph
buildGraph = foldr addEdge Map.empty
  where
    addEdge line graph = 
        let [a, b] = splitOn "-" line
            updateMap = Map.insertWith Set.union
        in updateMap b (Set.singleton a) $ updateMap a (Set.singleton b) graph

findTriangles :: Graph -> Set.Set Triplet
findTriangles graph = Set.fromList
    [ sortTriplet (n, x, y)
    | n <- Map.keys graph
    , x <- Set.toList (Map.findWithDefault Set.empty n graph)
    , y <- Set.toList (Map.findWithDefault Set.empty n graph)
    , x < y  -- Ensure we don't count the same pair twice
    , y `Set.member` (Map.findWithDefault Set.empty x graph)
    ]
  where
    sortTriplet (a, b, c) = let [x,y,z] = sort [a,b,c] in (x,y,z)

startsWithT :: Node -> Bool
startsWithT ('t':_) = True
startsWithT _ = False

hasNodeStartingWithT :: Triplet -> Bool
hasNodeStartingWithT (a, b, c) = any startsWithT [a, b, c]

solveFuncPart1 :: String -> Int
solveFuncPart1 input = length $ Set.filter hasNodeStartingWithT triangles
  where
    graph = buildGraph $ lines input
    triangles = findTriangles graph

-- Bron-Kerbosch algorithm implementation
bronKerbosch :: Graph -> Set.Set Node -> Set.Set Node -> Set.Set Node -> [Set.Set Node]
bronKerbosch graph r p x
    | Set.null p && Set.null x = [r]  -- Found a maximal clique
    | Set.null p = []  -- No more candidates
    | otherwise = concatMap explore (Set.toList p)
  where
    explore v =
        let neighbors = Map.findWithDefault Set.empty v graph
            r' = Set.insert v r
            p' = Set.intersection p neighbors
            x' = Set.intersection x neighbors
        in bronKerbosch graph r' p' x' ++
           bronKerbosch graph r (Set.delete v p) (Set.insert v x)

findLargestClique :: Graph -> Set.Set Node
findLargestClique graph = 
    let allNodes = Set.fromList $ Map.keys graph
        allCliques = bronKerbosch graph Set.empty allNodes Set.empty
    in if null allCliques 
       then Set.empty
       else maximum (map Set.size allCliques) `seq` maximumBy (comparing Set.size) allCliques

generatePassword :: Set.Set Node -> String
generatePassword clique = intercalate "," $ sort $ Set.toList clique

solveFuncPart2 :: String -> String
solveFuncPart2 input = generatePassword largestClique
  where
    graph = buildGraph $ lines input
    largestClique = findLargestClique graph
