import Data.List
import Control.Monad

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result
    
foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:xs) "*" = return (( x * y ):xs)
foldingFunction (x:y:xs) "+" = return (( x + y ):xs)
foldingFunction (x:y:xs) "-" = return (( x - y ):xs)
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _        -> Nothing

