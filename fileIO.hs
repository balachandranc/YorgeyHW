import System.IO
import Data.Char

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle