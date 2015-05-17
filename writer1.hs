import Control.Monad.Writer
import Data.Monoid

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
    a <- logNumber 3
    b <- logNumber 5
    return (a*b)

sumWithOp :: Writer (Sum Int) Int
sumWithOp = do
    x <- writer( 10, Sum 1 )
    y <- writer( 11, Sum 2 )
    tell $ Sum 3
    return (x + y)

main = putStrLn $ show $ runWriter multWithLog