import BFS
import Algebra.Graph.AdjacencyMap
import Data.Tree
main = do
    let g = overlays [1*2, 1*3, 1*4, 2*4, 3*5, 4*6, 5*6] :: AdjacencyMap Int
    -- let g = overlays [1*2, 1*3, 2*4, 4*3] :: AdjacencyMap Int
    let g2 = 1*(2+3) + 3*(6+7)+ 2*(4+5)+6*2+2*1+(4+2)*7 :: AdjacencyMap Int
    let g3 = 1 + 2*3 + 4*5*6 :: AdjacencyMap Int
    let g4 = (1*2)*3 + 4 + (5+6)*7 :: AdjacencyMap Int
    putStrLn $ show g
    putStrLn $ show $ bfsTreeAdjacencyMap 1 g
    putStrLn $ show $ bfsTreeAdjacencyMap 2 g2
    putStrLn $ show $ bfsTreeAdjacencyMap 8 g2
    putStrLn $ drawForest $ map (fmap show) (bfsForest g4)
    -- putStrLn $ drawForest $ map (fmap show) (bfsForest empty)