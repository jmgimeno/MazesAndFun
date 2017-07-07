module Grid where
  
import qualified Data.Graph.Inductive as Graph

type Wall = (Int, Int, Int, Int)
type Grid = Graph.Gr () Wall

gridNE :: Int -> Int -> Grid
gridNE width height = Graph.mkGraph nodes edges
  where nodes = [(n, ()) | x <- [0..width-1], 
                           y <- [0..height-1],
                           let n = xy2n' (x, y)] 
        edges = horizontal ++ vertical
        horizontal = [(n', n, (x, y', x+1, y')) 
                   | (n,  _) <- nodes,
                     (n', _) <- nodes,
                     let (x, y) = n2xy' n,
                     let (x', y') = n2xy' n',
                     x == x',
                     y + 1 == y']
        vertical = [(n, n', (x', y, x', y+1)) 
                     | (n,  _) <- nodes,
                       (n', _) <- nodes,
                       let (x, y) = n2xy' n,
                       let (x', y') = n2xy' n',
                       x + 1 == x',
                       y == y']
        xy2n' = xy2n width
        n2xy' = n2xy width
        
n2x :: Int -> Int -> Int
n2x = flip mod

n2y :: Int -> Int -> Int
n2y = flip div

n2xy :: Int -> Int -> (Int, Int)
n2xy w n = (n2x w n, n2y w n)

xy2n :: Int -> (Int, Int) -> Int
xy2n w (x, y) = w * y + x

walls :: Grid -> [Wall]
walls = map Graph.edgeLabel . Graph.labEdges