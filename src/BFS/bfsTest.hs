import BFS
import Algebra.Graph.AdjacencyMap
import Data.Tree
main = do
    let g = overlays [1*2, 1*3, 1*4, 2*4, 3*5, 4*6, 5*6] :: AdjacencyMap Int
    -- let g = overlays [1*2, 1*3, 2*4, 4*3] :: AdjacencyMap Int
    let g2 = 1*(2+3) + 3*(6+7)+ 2*(4+5)+6*2+2*1+(4+2)*7 :: AdjacencyMap Int
    let g3 = 1 + 2*3 + 4*5*6 :: AdjacencyMap Int
    -- putStrLn $ show g
    -- putStrLn $ show $ bfsAdjacencyMapTree 1 g
    -- putStrLn $ show $ bfsAdjacencyMapTree 2 g2
    -- putStrLn $ show $ bfsAdjacencyMapTree 8 g2
    -- putStrLn $ drawTree $ fmap show (bfsTree 2 g2)
    -- putStrLn $ drawTree $ fmap show (bfsTree 8 g2)
    putStrLn $ drawTree $ fmap show (bfsTree 2 (2*3 :: AdjacencyMap Int))
    putStrLn $ show $ bfsAdjacencyMapForest g3