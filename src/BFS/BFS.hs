module BFS (
    --Algorithms
    bfsTreeAdjacencyMap,
    bfsForest
) where

import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.AdjacencyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree

bfsTree :: Ord a => a -> AdjacencyMap a -> Tree a
bfsTree s g = unfoldTree neighbors s
    where neighbors b = (b, Set.toAscList . postSet b $ bfs)
          bfs = bfsTreeAdjacencyMap s g

bfsForest :: Ord a => AdjacencyMap a -> [Tree a]
bfsForest g
    | isEmpty g = []
    | otherwise = headTree : (bfsForest . induce remove) g
        where headTree = bfsTree ((head . vertexList) g) g
              remove x = not $ elem x $ flatten headTree

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
