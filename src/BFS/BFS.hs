module BFS (
    --Algorithms
    bfsTree
) where

import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.AdjacencyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

bfsTree :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
bfsTree s g = if (hasVertex s g) then bfsTreeUtil [s] (Set.singleton s) g else empty

bfsTreeUtil :: Ord a => [a] -> Set.Set a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) (bfsTreeUtil newQueue newSeen g)
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
        newSeen = Set.union seen neighbors
        newQueue = qv ++ (Set.toAscList vSet)