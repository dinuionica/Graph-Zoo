module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
 
    The crowd of the graph nodes.

    Hint: s.union
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
isMember :: Ord a => a -> S.Set a -> Bool
isMember desired_node nodes = S.member desired_node nodes

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

    CAREFUL!Do not use the EDGES function defined above because they would generate
    Too many unnecessary edges.

-}

inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = S.empty
inNeighbors node (Node a) = S.empty
inNeighbors node (Overlay a b) = S.union (inNeighbors node a) (inNeighbors node b)
inNeighbors node (Connect a b) 
    | isMember node (nodes b) = S.union (inNeighbors node b) (nodes a)
    | otherwise = S.union (inNeighbors node b) (inNeighbors node a)


{-
    Instant Class Num with the type (Algebraicgraph A), so:
    - a whole literal to be interpreted as a single knot with equal label
      with that literal
    - the gathering operation to be interpreted as overlay
    - The multiplication operation should be interpreted as Connect.

    The other functions in the class are not relevant.You will get warning
    For their non -implementation, but you can ignore them.

    After court, you will be able to evaluate expressions as:

    > 1 :: Algebraicgraph int
    Node 1
    
    > 1*(2+3) :: Algebraicgraph int
    Connect (Node 1) (Overlay (Node 2) (Node 3))
-}
instance Num a => Num (AlgebraicGraph a) where
    fromInteger = Node . fromInteger -- point free 
    (+) = Overlay
    (*) = Connect

{-
    Instant Class Show with the type (Algebraicgraph A) so that
    Representation in the form of a string of a graph to reflect
    arithmetic expressions defined above.You can put a new row of brackets
    at each compound subexpression.

    Examples:

    > Node 1
    1

    > Connect (Node 1) (Overlay (Node 2) (Node 3))
    (1*(2+3))
-}
instance Show a => Show (AlgebraicGraph a) where
    show Empty = ""
    show (Node a) = show a
    show (Overlay a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Connect a b) = "(" ++ show a ++ "*" ++ show b ++ ")"


{-
  
    Notice that the predefined EQ for type (Algebraicgraph A)
    does not correctly capture the equality of two graphs because the same graph
    Conceptual can have two different symbolic descriptions.
    
    Therefore, instantiate the EQ class with the type (Algebraicgraph A) so that
    Compare the crowds of knots and arches itself.

    Examples:

    > Node 1 == 1
    True

    > Node 1 == 2
    False

    > angle == 1*2 + 1*3
    True

    > triangle == (1*2)*3
    True
-}
instance Ord a => Eq (AlgebraicGraph a) where
    g1 == g2 = (nodes g1) == (nodes g2) && (edges g1) == (edges g2)

{-

    Extends an existing graph by attaching new arbitrary subographs instead of nodes
    individual.The function received as the first parameter determines this
    Correspondence between knots and subographs.Notice that the type of tags
    We (b) may be different from old labels (a).

    Example:

    > extend (\ n -> if n == 1 then 4+5 else node n) $ 1*(2+3)
    ((4+5)*(2+3))
-}
extend :: (a -> AlgebraicGraph b) -> AlgebraicGraph a -> AlgebraicGraph b
-- pattern matching
extend f Empty = Empty
extend f (Node a) = f a
extend f (Overlay a b) = Overlay (extend f a) (extend f b)
extend f (Connect a b) = Connect (extend f a) (extend f b)

{-

    Divide a knot into several nodes with the elimination of the original node.
    The arcs in which the old knot was involved must become valid
    for the new knots.
    
    Implement Splitnode using Extend!
-}


addNodesToGraph :: Eq a
           => [a]           
           -> AlgebraicGraph a -- graful initial

addNodesToGraph targets = if (length targets) == 0 then Empty -- lista vida
                 else if (length targets) == 1 then Node (head targets) -- un singur element nou
                 else if (length targets) ==  2 then Overlay (Node (head targets)) (Node (last targets)) -- doua elemente noi
                 else Overlay (Overlay (Node (head targets)) (Node (head(tail targets)))) (Node (last targets))         


splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode node targets graph = extend (\node_target -> if node_target == node then (addNodesToGraph targets) 
                                                       else Node node_target) graph

{-
    Institute the functional class with the algebraicgraph type builder, thus
    that you can apply a function on all labels of a graph.
    FMAP represents the generalization of MAP for any kind of structure.

    Implement FMAP using extend!

    Example:

    > FMAP (+ 10) $ 1*(2+ 3) :: Algebraicgraph int
    (11*(12+13))
-}

instance Functor AlgebraicGraph where
    -- fmap :: (a -> b) -> AlgebraicGraph a -> AlgebraicGraph b
    fmap f graph = extend (\node -> Node $ f node) graph
{-

    Combines multiple knots in one, based on a property
    Respected by the joined nodes, with their elimination.The arches in which
    The old nodes were involved will refer to the new node.

    Implement Mapnodes using FMAP!
-}

mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node = fmap (\old_node -> if prop old_node then node else old_node)

{-

    Filter a graph, keeping only the nodes that satisfy the given property.

    Implement Filtergraph using Extend!
    
    Example:
    > nodes $ filterGraph odd $ 1*(2+3)
    fromList [1,3]

    > edges $ filterGraph odd $ 1*(2+3)
    fromList [(1,3)]
-}
filterGraph :: (a -> Bool) -> AlgebraicGraph a -> AlgebraicGraph a
filterGraph prop graph = extend (\node -> if prop node then (Node node) else Empty) graph

{-
    Turns the resulting graph by removing a knot and arches in which
    It is involved.If the node does not exist, the same graph returns.

    Implement Removenode using Filtergraph!
-}

removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = filterGraph (/= node) graph
