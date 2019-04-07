-- No existe forma de de representar GRAFOS CONECTADOS. Esto implica que 1) Kruskall no puede ser implementado 2) El grafo resultante puede tener islas

import Algebra.Graph.Labelled.AdjacencyMap
import Algebra.Graph.Labelled.AdjacencyMap.Internal
import Algebra.Graph.Label

import qualified Data.Map.Strict                     as Map
import qualified Data.Set                            as Set
import qualified Algebra.Graph.AdjacencyMap          as AM
import qualified Algebra.Graph.AdjacencyMap.Internal as AMI
import Data.Monoid
import Data.List

sumWeights :: (Num e, Ord a) => AdjacencyMap e a -> e
sumWeights g = foldr (+) 0 (map (\(x,_,_) -> x) (edgeList g))

main = do
    let g = overlays [ edge 3 1 2, edge 4 1 5, edge 4 2 3, vertex 6 ] :: AdjacencyMap (Distance Int) Int
    putStrLn $ show $ sumWeights g
    putStrLn $ show $ sort (edgeList g)

getSortedEdges :: (Monoid e, Ord a, Ord e) => AdjacencyMap e a -> [(e,a,a)]
getSortedEdges = (sortBy (\(w1,_,_) (w2,_,_) -> if w1 < w2 then LT else GT) . edgeList)