{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Data StandardGraph g 
        = Empty
        | Node g
        | Edge g
        fromComponent g = (Node g, Edge g)
-}

{-
    Graf -oriented graph with type A knots, represented by crowds (set)
    of knots and arches.

    Crowds are useful because they manage duplicates and allow
    Testing equality of two graphs without taking into account the order of the nodes
    and the arches.

    Type introduces a type synonym, similar to Typedef from C.
-}

type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    Build a graph based on nodes and arches lists.
    Constraint (order a) states that type A values must be
    Ordinary, which is necessary for the internal representation of the crowds.
    It is just a detail, with which you will not explicitly operate at this stage.
    You will meet this constraint in the types of the functions below.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    The crowd of the graph nodes.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst

{-
    The crowd of the graphic arches.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Examples of graphs
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    The crowd of the knots to which arches leave from the current node.

    Example:

    > Outneighbors 1 Graph3
    Fromlist [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.fromList (foldr (\pair acc -> snd pair : acc) 
                                     [] (filter (\(x, y) -> x == node) (S.toList (edges graph))))

{-
    The crowd of the nodes from which Arrce leaves to the current knot.

    Example:

    > Inneighbors 1 Graph3
    Fromlist [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.fromList (foldr (\pair acc -> fst pair : acc) 
                                     [] (filter (\(x, y) -> y == node) (S.toList(edges graph))))

{-
   The auxiliary function for the wipes of the nodes
-}
removeNodes :: Ord a => a -> StandardGraph a -> S.Set a
removeNodes node graph = S.filter (\x -> x /= node) (nodes graph)

{-
   Auxiliary function to delete the edges
-}
removeEdges :: Ord a => a -> StandardGraph a -> S.Set (a, a)
removeEdges node graph = S.filter (\(x, y) -> x /= node && y /= node) (edges graph)

{-
    Turn the resulting graph by removing a knot and arches in which
    It is involved.If the node does not exist, turn the same graph.

    Example:

    > Removenode 1 Graph3
    (Fromlist [2,3,4], Fromlist [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = ((removeNodes node graph), (removeEdges node graph))

{-
    Auxiliary functions to determine the neighbors of a knot
-}
addoutNeighbors :: Ord a => a -> StandardGraph a -> [a] -> S.Set (a, a)
addoutNeighbors old graph news = S.fromList (foldr (\y acc -> (map (\x -> (y, x)) 
                                            (S.toList(outNeighbors old graph))) ++ acc) [] news)

addInNeighbors :: Ord a => a -> StandardGraph a -> [a] -> S.Set (a, a)
addInNeighbors old graph news = S.fromList (foldr (\y acc -> (map (\x -> (x, y)) 
                                           (S.toList(inNeighbors old graph))) ++ acc) [] news)

addAllNeighbors :: Ord a => a -> StandardGraph a -> [a] -> S.Set (a, a)
addAllNeighbors old graph news = S.union (addoutNeighbors old graph news) (addInNeighbors old graph news)

{-
    Divide a knot into several nodes with the elimination of the original node.
    The arcs in which the old knot was involved must become valid
    for the new knots.

    Example:

    > Splitnode 2 [5,6] Graph3
    (Fromlist [1,3,4,5,6], Fromlist [(1,3), (1.4), (1.5), (1.6), (4.1), (5.3), (6.3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut

splitNode old news graph = (S.union (removeNodes old graph) (S.fromList news),
                            S.union (removeEdges old graph) (addAllNeighbors old graph news))                            
                            
{-
    The function that sterger nodes that do not respect the desired property
    ANY function check if at least one item in the list checks a property
-}
removeNodesByProp :: Ord a => (a -> Bool) -> a -> StandardGraph a -> S.Set a
removeNodesByProp prop node graph = S.fromList (filter (\x -> not (prop x)) (S.toList(nodes graph)) 
                                               ++ if (any prop (nodes graph)) then [node] else [])

{-
    The function that updates the edges of the stepped nodes with the new knot
-}
changeEdgesWithNewNode :: Ord a => (a -> Bool) -> a -> StandardGraph a -> S.Set (a, a)
changeEdgesWithNewNode prop node graph = S.fromList (foldr (\x acc ->  
                                         if (prop (fst x) && prop(snd x)) then ((node, node) : acc)
                                         else if (prop (snd x)) then ( (fst x, node) : acc) 
                                         else if (prop (fst x)) then( (node, (snd x)) : acc)  
                                         else (x : acc)) [] (S.toList(edges graph)))

{-
    Combines multiple knots in one, based on a property
    Respected by the joined nodes, with their elimination.The arches in which
    The old nodes were involved will refer to the new node.

    Example:

    > Mapnodes even 5 Graph3
    (Fromlist [1,3,5], Fromlist [(1,3), (1.5), (5.1), (5.3)])
-}
mergeNodes :: order a
           => (a -> bool) -the property fulfilled by the joined nodes
           -> a -the new knot
           -> Standardgraph A -existing graph
           -> Standardgraph A -Graph obtained
mergeNodes prop node graph =  (removeNodesByProp prop node graph, changeEdgesWithNewNode prop node graph)
