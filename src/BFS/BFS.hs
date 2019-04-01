module BFS (
    --Algorithms
    bfsTreeAdjacencyMap,
    -- bfsForestAdjacencyMap,
    bfsForest
) where

import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.AdjacencyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree
import Data.Maybe

bfsForest :: Ord a => AdjacencyMap a -> Forest a
bfsForest g= map (\x -> bfsTree (firstV x) x) (bfsForestAdjacencyMap g)
    where firstV = head . vertexList


bfsTree :: Ord a => a -> AdjacencyMap a -> Tree a
bfsTree s g = unfoldTree look s
    where look b = (b, (Set.toAscList . fromJust . Map.lookup b) bfs)
          bfs = adjacencyMap (bfsTreeAdjacencyMap s g)

bfsForestAdjacencyMap :: Ord a => AdjacencyMap a -> [AdjacencyMap a]
bfsForestAdjacencyMap g
    | isEmpty g = []
    | otherwise = headTree : bfsForestAdjacencyMap (induce (\x -> not (hasVertex x headTree)) g)
        where headTree = bfsTreeAdjacencyMap ((head . vertexList) g) g

bfsTreeAdjacencyMap :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeAdjacencyMap s g = if (hasVertex s g) 
                          then bfsTreeUtil [s] (Set.singleton s) g 
                          else empty

bfsTreeUtil :: Ord a => [a] -> Set.Set a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) (bfsTreeUtil newQueue newSeen g)
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
        newSeen = Set.union seen neighbors
        newQueue = qv ++ (Set.toAscList vSet)
