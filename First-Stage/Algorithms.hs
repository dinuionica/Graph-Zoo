module Algorithms where

import Data.List
import qualified Data.Set as S
import StandardGraph
import Data.Maybe

{-
    In stage 1, through graph we mean a standard representation graph.
    In the following stages, we will experience with another representation.

    Type introduces a type synonym, similar to Typedef from C.
-}
Type Graph A = Standardgraph A

{-
    *** Todo ***
    General function that abstract BFS and DFS starting from a particular knot,
    by the function of combining the lists that constitute the first parameter.
    
    The two lists received as a parameter of the combination function are the list
    elements already in structure (tail/stack), respectively the list
    the neighbors of the current, freshly expanded node.

    The search should take into account any cycles.

    Hint: Write an auxiliary function that receives as an additional parameter
    a lot (set) that retains the visited nodes to the current moment.
-}

{-
    The function that determines the neighboring knots of a knot
-}
neighbours:: Ord a => a -> Graph a -> [a]
neighbours element graph = S.toList (outNeighbors element graph)

auxiliary_search:: Ord a
                => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
                -> Graph a              -- graful
                -> S.Set a              -- setul de noduri vizitate
                -> [a]                  -- structura de date -> coada / stiva
                -> [a]                  -- lista de noduri
                -> [a]                  -- lista obținută în urma parcurgerii
        
auxiliary_search f graph visited ds result_list 
    | null ds = result_list 
    | (S.member (head ds) visited) = auxiliary_search f graph visited (tail ds) result_list 
    | otherwise = auxiliary_search f graph (S.insert (head ds) visited) 
                  (f (tail ds) (neighbours (head ds) graph)) (result_list ++ [head ds])
    

search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = (auxiliary_search f graph S.empty [node] [])

{-

    The BFS strategy, derived by partial application of the search function.

    Examples:

    > BFS 1 Graph4
    [1,2,3,4]

    > BFS 4 Graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
bfs = search (\list1 list2 -> list1 ++ list2)

{-
    The DFS strategy, derived by partial application of the search function.

    Examples:

    > DFS 1 Graph4
    [1,2,4,3]
    
    > DFS 4 Graph4
    [4,1,2,3]
-}

dfs :: Ord a => a -> Graph a -> [a]
dfs = search (\list1 list2 -> list2 ++ list1); dfs =


{-
    The function that determines the index of a number within a list
-}
findPositionList:: Eq a => a -> [a] -> Int
findPositionList elem list = fromJust (elemIndex elem list)

{-
    The function counts how many intermediate nodes expands BFS strategies,
    respectively DFS, in trying to find a path between a source knot
    and one destination, taking into account the possibility of its absence from the graph.
    The number excludes source and destination nodes.

    The usual way in Haskell to specify that a function may not be
    defined for certain parameter values is the type builder
    Maybe.Thus, if a path exists, the function returns
    Just (number, number, number), and otherwise, nothing.

    Examples:

    > Countytermediete 1 3 graph4
    Just (1,2)

    Here, BFS from Node 1 returns [1,2,3,4], so there is only one knot
    Intermediate (2) between 1 and 3. DFS returns [1,2,4,3], so there are two nodes
    Intermediate (2, 4) between 1 and 3.

    > Countytermediate 3 1 Graph4
    Nothing

    There is no way between 3 and 1.
-}
countIntermediate :: Ord a
                  -> a - source node
                  -> a -node destination
                  -> Standardgraph A -Graph
                  -> Maybe (int, int) -the number of nodes expanded by BFS/DFS
                  
countIntermediate from to graph = 
    let bfs_list = bfs from graph
        dfs_list = dfs from graph
    in 
        if (elem to bfs_list && elem to dfs_list) then 
            Just ((findPositionList to bfs_list ) - 1, (findPositionList to dfs_list ) - 1)
        else 
            Nothing
