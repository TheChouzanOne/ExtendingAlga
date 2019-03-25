import BFS
import Algebra.Graph.AdjacencyMap
main = do
    let g = overlays [1*2, 1*3, 1*4, 2*4, 3*5, 4*6, 5*6] :: AdjacencyMap Int
    -- let g = overlays [1*2, 1*3, 2*4, 4*3] :: AdjacencyMap Int
    putStrLn $ show g
    putStrLn $ show $ bfsTree 1 g
    putStrLn $ show $ bfsTree 2 (1*(2+3) + 3*(6+7)+ 2*(4+5)+6*2+2*1+(4+2)*7 :: AdjacencyMap Int)