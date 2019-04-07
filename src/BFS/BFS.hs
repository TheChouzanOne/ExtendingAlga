module BFS (
    --Algorithms
    bfsForest
) where

import Algebra.Graph.AdjacencyMap.Internal as AM
import Algebra.Graph.AdjacencyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tree
import Data.List

bfs2 :: Ord a => [a] -> AdjacencyMap a -> [[a]]
bfs2 vs g = foldr (zipWith (++)) acc (map (++ repeat []) l)
    where l = bfs vs g 
          maxLength = maximum (map length l)
          acc = [ [] | _<-[1..maxLength]]




bfs :: Ord a => [a] -> AdjacencyMap a -> [[[a]]]
bfs vs = (map levels . bfsForestFrom vs)


-- bfsForestFrom vs 'empty'                                      == []
-- 'forest' (bfsForestFrom [1]   $ 'edge' 1 1)                   == 'vertex' 1
-- 'forest' (bfsForestFrom [1]   $ 'edge' 1 2)                   == 'edge' 1 2
-- 'forest' (bfsForestFrom [2]   $ 'edge' 1 2)                   == 'vertex' 2
-- 'forest' (bfsForestFrom [3]   $ 'edge' 1 2)                   == 'empty'
-- 'forest' (bfsForestFrom [2,1] $ 'edge' 1 2)                   == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForestFrom vs x) x              == True
-- bfsForestFrom ('vertexList' x) x                              == 'bfsForest' x
-- bfsForestFrom vs             ('vertices' vs)                  == 'map' (\\v -> Node v []) ('Data.List.nub' vs)
-- bfsForestFrom []             x                                == []
-- bfsForestFrom [1,4] $ 1 * (3+5+7) + 3 * (5+4) + (4+3+5+7) * 6 ==  [ Node { rootLabel = 3
--                                                                          , subForest = [ Node { rootLabel = 4
--                                                                                                 , subForest = []}
--                                                                                        , Node { rootLabel = 5
--                                                                                               , subForest = []}
--                                                                                        , Node { rootLabel = 6
--                                                                                               , subForest = [] }]}
--                                                                   , Node { rootLabel = 1
--                                                                          , subForest = [ Node { rootLabel = 7
--                                                                                               , subForest = [] }]}]
-- @
bfsForestFrom :: Ord a => [a] -> AdjacencyMap a -> Forest a
bfsForestFrom [] _ = []
bfsForestFrom (v:vs) g
    | hasVertex v g = headTree:bfsForestFrom vs (induce (\x -> not $ elem x removedVertices) g)
    | otherwise = bfsForestFrom vs g
        where headTree = bfsTree v g
              removedVertices = flatten headTree
-- | Compute the /breadth-first search/ forest of a graph that corresponds to
-- searching from each of the graph vertices in the 'Ord' @a@ order.
--
-- @
-- bfsForest 'empty'                       == []
-- 'forest' (bfsForest $ 'edge' 1 1)         == 'vertex' 1
-- 'forest' (bfsForest $ 'edge' 1 2)         == 'edge' 1 2
-- 'forest' (bfsForest $ 'edge' 2 1)         == 'vertices' [1,2]
-- 'isSubgraphOf' ('forest' $ bfsForest x) x == True
-- 'isbfsForestOf' (bfsForest x) x         == True
-- bfsForest . 'forest' . bfsForest        == bfsForest
-- bfsForest ('vertices' vs)               == 'map' (\\v -> Node v []) ('Data.List.nub' $ 'Data.List.sort' vs)
-- bfsForest $ 1 * (3+5+7) + 3 * (5+4) + (4+3+5+7) * 6 ==  [Node {rootLabel = 1
--                                                               , subForest = [Node {rootLabel = 3
--                                                                                   , subForest = [ Node {rootLabel = 4
--                                                                                                        , subForest = [] }
--                                                                                                 , Node {rootLabel = 6
--                                                                                                        , subForest = [] }]}
--                                                                             , Node {rootLabel = 5
--                                                                                    , subForest = [] }
--                                                                             , Node {rootLabel = 7
--                                                                                    , subForest = [] }]}]
-- @
bfsForest :: Ord a => AdjacencyMap a -> [Tree a]
bfsForest g
    | isEmpty g = []
    | otherwise = headTree : (bfsForest . induce remove) g
        where headTree = bfsTree ((head . vertexList) g) g
              remove x = not $ elem x $ flatten headTree


-- | Compute the /breadth-first search/ AdjacencyMap of a graph that corresponds to
-- searching from a single vertex of the graph. 
-- This is just for internal use. Might move it to `*.Internal` then?
--
bfsTreeAdjacencyMap :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeAdjacencyMap s g = if (hasVertex s g) 
                          then bfsTreeAdjacencyMapUtil [s] (Set.singleton s) g 
                          else empty

-- | Compute the /breadth-first search/ AdjacencyMap of a graph that corresponds to
-- searching from the head of a queue (followed by other vertices to search from), 
-- given a Set of seen vertices (vertices that shouldn't be visited).
-- This is just for internal use. Might move it to `*.Internal` then?
--
bfsTreeAdjacencyMapUtil :: Ord a => [a] -> Set.Set a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeAdjacencyMapUtil [] _ _ = empty
bfsTreeAdjacencyMapUtil queue@(v:qv) seen g = overlay (AM.AM $ Map.singleton v vSet) (bfsTreeAdjacencyMapUtil newQueue newSeen g)
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
        newSeen = Set.union seen neighbors
        newQueue = qv ++ (Set.toAscList vSet)

-- | Compute the /breadth-first search/ Tree of a graph that corresponds to
-- searching from a single vertex of the graph. This is just for internal use. 
-- Might move it to `*.Internal` then?
--
bfsTree :: Ord a => a -> AdjacencyMap a -> Tree a
bfsTree s g = unfoldTree neighbors s
    where neighbors b = (b, Set.toAscList . postSet b $ bfs)
          bfs = bfsTreeAdjacencyMap s g
