import System.Random
import Control.Monad

main = do
    gen <- getStdGen
    askForNumber gen


askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)
    putStrLn "What number am I thinking of ?"
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
            then putStrLn "You guessed right!"
            else putStrLn $ "Sorry the number was " ++ show randNumber
        askForNumber newGen