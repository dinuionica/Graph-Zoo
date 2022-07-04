module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    A partition is a lot of sub -meters of another set, disjoint
    (without common elements) and which together contain all the original elements.
    
    For example, for the crowd [1,2,3], a possible partition is [1], [2,3]].

-}
type Partition a = S.Set (S.Set a)

{-
    Apply a function on each element of a list, but only on one
    At one point, keeping the other unchanged.Therefore for
    Each item in the initial list results in a list in the final list,
    related to the modification of only that element.

    Example:

    > mapSingle (+10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}
-- foldr (\x acc -> (foldr (\y acc2 -> if x == y then (f x) : acc2 else y : acc2) [] xs) : acc) [] xs

-- foldr (\x acc-> ((fst (splitAt pos xs) ++ [f x]) ++ (tail (snd (splitAt pos xs)))) :  acc) [] xs
    

{-
    Return a list of lists obtained by combating value_head value at each list
-}
concatenateHeadToList :: a -> [[a]] -> [[a]]
concatenateHeadToList value_head list_list = map (value_head : ) list_list

{-
  - recursive mapsingle
    - If the list length is null, the Vida list is returned
    - the list in which f is applied only on the first item in the list
    - The result list of lists obtained from the recursive appeal is concited,
    at each list of concatenandu -the head is

    (10 + 1) : [2, 3] => [11, 2, 3] ++
    [1 : (10 + 2) : [3]] => [1, 12, 3] ++
    [1, 2] : (10 + 3) => [1, 2, 13]

-}
mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = 
    if (length xs) == 0 then [] 
    else [(f (head xs) : (tail xs))] ++ (concatenateHeadToList (head xs) (mapSingle f (tail xs)))

{-
    Apply a function on each element of a list, but only on one
    At one point, keeping the other unchanged.Therefore for
    Each item in the initial list results in a list in the final list,
    related to the modification of only that element.

    Example:

    > mapSingle' (+10) [1,2,3]
    [11,12,13]
    
-}

{- 
    If you find it hard to follow the type turned on the function, with 3 levels
    of the list of list-type builder, think so:
    - We need a level for a sub -meal
    - Another level for a partition, which is a lot of sub -sciences
    - Another level for the crowd of all partitions.

    Hint: Use List Comprehensions to answer the question:
    If we have obtained a partition of the rest of the list, how do we get a partition
    of the entire list, which includes the head?(use also mapsingle) `

    Examples:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}

{-
    - Recursive partitions
    - Choose a partition, to which the head of the list is added (a new part is started)
    [1], [2], [3]
    - then add head to a lot already existence
    - Mapsingle is applied to Concatena Head to one of the crowds in the partition
    - applies conquered because the result is [[[[]]]] and the desired result is a
    list [[[[[]]]]

 -}
partitions :: [a] -> [[[a]]]
partitions xs = 
    let 
        head_xs = head xs
        partitions_tail = (partitions (tail xs))
    in
        if length(xs) == 1 then [[[head_xs]]] -- cazul de baza
        else [[head_xs] : p | p <- partitions_tail] ++
            (concat [mapSingle (head_xs : ) p | p <- partitions_tail])

