import Data.Array

data Label = FREE | UP | DOWN | LEFT | RIGHT
    deriving (Eq, Show, Enum, Bounded)

type Size = (Int, Int)

type Maze = Array Size Label

createEmptyMaze :: Size -> Maze
createEmptyMaze (width,height) = array ( (1,1), (height,width) ) [ ( (row,col), FREE ) | row <- [1..height], col <- [1..width] ]