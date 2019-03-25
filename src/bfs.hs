import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.AdjacencyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

bfsTree :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
bfsTree s g = bfsTreeUtil [s] (Set.singleton s) g

bfsTreeUtil :: Ord a => [a] -> Set.Set a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeUtil [] _ g = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) (bfsTreeUtil newQueue newSeen g)
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
        newSeen = Set.union seen neighbors
        newQueue = qv ++ (Set.toAscList vSet)


main = do
    let g = overlays [1*2, 1*3, 1*4, 2*4, 3*5, 4*6, 5*6] :: AdjacencyMap Int
    -- let g = overlays [1*2, 1*3, 2*4, 4*3] :: AdjacencyMap Int
    putStrLn $ show g
    putStrLn $ show $ bfsTree 1 g
    putStrLn $ show $ bfsTree 2 (1*(2+3) + 3*(6+7)+ 2*(4+5)+6*2+2*1+(4+2)*7 :: AdjacencyMap Int)