import Data.Array
import Data.List
import System.Random
import Control.Monad

data Label = FREE | ROOT | UP | DOWN | LEFT | RIGHT
    deriving (Eq, Show, Enum, Bounded)

type Size = (Int, Int)

type Maze = Array Size Label

type Vertex = (Int, Int)

type Path = [Vertex]


createEmptyMaze :: Size -> Maze
createEmptyMaze (width,height) = array ( (1,1), (height,width) ) [ ( (row,col), FREE ) | row <- [1..height], col <- [1..width] ]

pickRandom :: [a] -> IO a
pickRandom xs = do
    randIndex <- randomRIO ( 0, length xs - 1 )
    return $ xs !! randIndex

visitRandom :: [Vertex] -> [Vertex] -> IO (Vertex,[Vertex],[Vertex])
visitRandom unvisited visited = do
    selected <- pickRandom unvisited
    return (selected, delete selected unvisited, selected : visited )

allowedDirs :: (Vertex,Vertex) -> Vertex -> [Label]
allowedDirs ((minX,minY),(maxX,maxY)) (x,y) = do
    (cond,label) <- [ (x > minX, LEFT), ( x < maxX, RIGHT ), ( y > minY, UP ), ( y < maxY, DOWN ) ]
    guard cond
    return label

moveVertex :: Vertex -> Label -> Vertex
moveVertex (x,y) UP = (x,y-1)
moveVertex (x,y) DOWN = (x,y+1)
moveVertex (x,y) LEFT = (x-1,y)
moveVertex (x,y) RIGHT = (x+1,y)

nextVertex :: Maze -> Vertex -> IO Vertex
nextVertex maze vert = do
    let dirs = allowedDirs (bounds maze) vert
    randDir <- pickRandom dirs
    return $ moveVertex vert randDir

randomPath :: Maze -> Path -> Vertex -> IO Path
randomPath maze path vertex = do
    if maze ! vertex /= FREE
        then return $ path ++ [vertex]
        else do
            vert <- nextVertex maze vertex
            putStrLn $ show $ length path
            randomPath maze (path ++ [vertex]) vert

solveMaze' :: Maze -> [Vertex] -> [Vertex] -> IO Maze
solveMaze' maze unvisited visited = do
    if length unvisited == 0
        then return maze
        else do
            startVertex <- pickRandom unvisited
            path <- randomPath maze [] startVertex
            putStrLn $ show path
            return maze

solveMaze :: Maze -> IO Maze
solveMaze maze = do
    let unvisited = range $ bounds maze
    (rootVertex, unvisited, visited) <- visitRandom unvisited []
    putStrLn $ "rootVertex: " ++ show rootVertex
    let maze' = maze // [(rootVertex, ROOT)]
    solveMaze' maze' unvisited visited

main = do
    let maze = createEmptyMaze (6,4)
    solvedMaze <- solveMaze maze
    putStrLn $ show solvedMaze