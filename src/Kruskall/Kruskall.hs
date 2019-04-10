import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Labelled.AdjacencyMap.Internal
import Algebra.Graph.Label

import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set
import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AMI
import Data.Monoid
import Data.List
import Data.Maybe

getSortedEdgesBy :: (Monoid e, Ord a, Ord e) => (e->e->Ordering) -> AdjacencyMap e a -> [(e,a,a)]
getSortedEdgesBy f = (sortBy (\(w1,_,_) (w2,_,_) -> f w1 w2) . edgeList)

kruskall :: (Monoid e, Ord e, Ord a, Eq a) => AdjacencyMap e a -> AdjacencyMap e a
kruskall g =  kruskallUtil edges initialMap initialG
    where edges = getSortedEdgesBy (\x y -> if x > y then GT else LT) g
          initialMap = foldr Map.union Map.empty (map (\x -> Map.singleton x x) (vertexList g))
          initialG = vertices $ vertexList g

kruskallUtil :: (Monoid e, Ord a, Eq a, Ord e) => [(e, a, a)] -> Map.Map a a -> AdjacencyMap e a -> AdjacencyMap e a
kruskallUtil [] _ g = g
kruskallUtil ((e,x,y):es) ds g = case valX == valY of -- I could also implement it using disjoint from Data.Sets, but I believe the cost would increase.
    True -> kruskallUtil es ds g
    False -> kruskallUtil es newDS newG
    where newDS = Map.map (\v -> if v == valY then valX else v) ds --Basically path compression
          newG = overlay g (edge e x y)
          valX = fromJust $ Map.lookup x ds
          valY = fromJust $ Map.lookup y ds