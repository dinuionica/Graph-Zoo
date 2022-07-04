module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
W
    Mulțimea nodurilor grafului.

    Hint: S.union
-}

nodes :: Ord a => AlgebraicGraph a -> S.Set a

nodes Empty = S.empty
nodes (Node a) = S.singleton a
nodes (Overlay a b) = S.union (nodes a) (nodes b)
nodes (Connect a b) = S.union (nodes a) (nodes b)


{-
    The crowd of the graphic arches.

    Hint: s.union, s.cartesianproduct
-}

edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay a b) = S.union (edges a) (edges b)
edges (Connect a b) = S.union (S.cartesianProduct (nodes a) (nodes b)) (S.union (edges a) (edges b))


- the function that checks whether a node is a member of a set
ismember :: order a => a -> s.Set a -> bool
ismember desired_node nodes = s.member desired_node nodes

{-
    The crowd of the knots to which arches leave from the current node.

    CAREFUL!Do not use the EDGES function defined above because they would generate
    Too many unnecessary edges.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = S.empty
outNeighbors node (Node a) = S.empty
outNeighbors node (Overlay a b) = S.union (outNeighbors node a) (outNeighbors node b)
outNeighbors node (Connect a b) 
    | (isMember node (nodes a)) = S.union (outNeighbors node a) (nodes b)
    | otherwise = S.union (outNeighbors node a) (outNeighbors node b)

{-
    The crowd of the nodes from which Arrce leaves to the current knot.

-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node a) = S.empty
inNeighbors node (Overlay a b) = S.union (inNeighbors node a) (inNeighbors node b)
inNeighbors node (Connect a b) 
    | isMember node (nodes b) = S.union (inNeighbors node b) (nodes a)
    | otherwise = S.union (inNeighbors node b) (inNeighbors node a)

{-
    
    Turns the resulting graph by removing a knot and arches in which
    It is involved.If the node does not exist, the same graph returns.

    Hint: Define a local recursive function (eg in WHERE),
    to receive as parameters only variable entities from a call
    recursively to another so you do not copy to each call parameters
    unchanged.For example, the Node parameter does not change, while
    The graph parameter changes.

-}

removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node Empty = Empty
removeNode node (Node a)
    | node == a = Empty
    | otherwise = Node a
removeNode node (Overlay a b) = Overlay (removeNode node a) (removeNode node b)
removeNode node (Connect a b) = Connect (removeNode node a) (removeNode node b)

   
{-
    Divide a knot into several nodes with the elimination of the original node.
    The arcs in which the old knot was involved must become valid
    for the new knots.
    
    Hint: Local recursive function, as in Removenode.
-}

splitNode :: Eq a
          => a - divided knot
          -> [a] -the nodes with which it is replaced
          -> algebraicgraph a -existing graph
          -> algebraicgraph a -the graph obtained
splitNode old news Empty = Empty
splitNode old news (Node a)
    | old == a = if (length news) == 0 then Empty -- lista vida
                 else if (length news) == 1 then Node (head news) -- un singur element nou
                 else if (length news) ==  2 then Overlay (Node (head news)) (Node (last news)) -- doua elemente noi
                 else Overlay (Overlay (Node (head news)) (Node (head(tail news)))) (Node (last news))         
    | otherwise = Node a
splitNode old news (Overlay a b) = Overlay (splitNode old news a) (splitNode old news b)
splitNode old news (Connect a b) = Connect (splitNode old news a) (splitNode old news b)


{-
    Combines multiple knots in one, based on a property
    Respected by the joined nodes, with their elimination.The arches in which
    The old nodes were involved will refer to the new node.

    Hint: Local recursive function, as in Removenode.
-}

mergeNodes :: (a -> bool) -the property fulfilled by the joined nodes
           -> a -the new knot
           -> algebraicgraph a -existing graph
           -> algebraicgraph a -the graph obtained
mergeNodes property new Empty = Empty
mergeNodes property new (Node a)
    | property a = Node new -- daca se respecta proprietatea modific nodul curent
    | otherwise = Node a    -- altfel pastrez vechiul nod
mergeNodes property new (Overlay a b) = Overlay (mergeNodes property new a) (mergeNodes property new b)
mergeNodes property new (Connect a b) = Connect (mergeNodes property new a) (mergeNodes property new b)
    