{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Array
import Data.List
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Random
import System.IO

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
moveVertex (x,y) UP     = (x,y-1)
moveVertex (x,y) DOWN   = (x,y+1)
moveVertex (x,y) LEFT   = (x-1,y)
moveVertex (x,y) RIGHT  = (x+1,y)

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
            randomPath maze (path ++ [vertex]) vert

simplifyPath :: Path -> Path
simplifyPath [] = []
simplifyPath (x:xs) = let simpleTail = simplifyPath xs
                      in case elemIndex x simpleTail of
                            Nothing     -> x:simpleTail
                            Just index  -> snd $ splitAt index simpleTail

dirsFromIndices :: Vertex -> Vertex -> Label
dirsFromIndices (y1,x1) (y2,x2) = case ((x2-x1),(y2-y1)) of
                                        (0,-1) -> UP
                                        (0,1)  -> DOWN
                                        (-1,0) -> LEFT
                                        (1,0)  -> RIGHT

dirsFromPath :: Path -> [Label]
dirsFromPath [] = []
dirsFromPath [x] = []
dirsFromPath (x:y:xs) = dirsFromIndices x y : dirsFromPath (y:xs)

updateMaze :: Maze -> Path -> [Label] -> Maze
updateMaze maze path labels = maze // zip path labels

solveMaze' :: Maze -> [Vertex] -> [Vertex] -> IO Maze
solveMaze' maze unvisited visited = do
    if length unvisited == 0
        then return maze
        else do
            startVertex <- pickRandom unvisited
            path <- randomPath maze [] startVertex
            let simplifiedPath = simplifyPath path
            putStrLn $ show simplifiedPath
            let pathDirs = dirsFromPath simplifiedPath
            putStrLn $ show pathDirs
            let maze' = updateMaze maze simplifiedPath pathDirs
            solveMaze' maze' ( unvisited \\ simplifiedPath ) ( visited ++ simplifiedPath ) 

solveMaze :: Maze -> IO Maze
solveMaze maze = do
    let unvisited = range $ bounds maze
    (rootVertex, unvisited, visited) <- visitRandom unvisited []
    putStrLn $ "rootVertex: " ++ show rootVertex
    let maze' = maze // [(rootVertex, ROOT)]
    solveMaze' maze' unvisited visited

mazeWidth :: Maze -> Int
mazeWidth maze = snd $ snd $ bounds maze

mazeHeight :: Maze -> Int
mazeHeight maze = fst $ snd $ bounds maze 

borderRow :: Int -> String
borderRow width = ( '+' : take width (repeat '-') ) ++ ['+']

showMaze :: Maze -> String
showMaze maze = unlines $ borderRowString ++ (map showRow rows) ++ borderRowString
                where 
                    borderRowString = [ borderRow ( 2 * width - 1 ) ]
                    width = mazeWidth maze
                    showRow row = ( '|' : intersperse '|' ( showRow' row ) ) ++ ['|']
                    showRow' row = map lookupSymbol row
                    lookupSymbol x = case x of
                                        LEFT    -> 'L'
                                        RIGHT   -> 'R'
                                        UP      -> 'U'
                                        DOWN    -> 'D'
                                        FREE    -> ' '
                                        ROOT    -> 'O'
                    rows = map (\i -> [ maze ! (i,j) | j <- [1..width]]) [1..(mazeHeight maze)]


main = do
    let maze = createEmptyMaze (6,4)
    solvedMaze <- solveMaze maze
    putStrLn $ showMaze solvedMaze