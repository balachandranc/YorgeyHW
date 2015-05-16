import Control.Monad

main = forever $ do
    c <- getChar
    if c == ' '
        then return ()
        else putChar c