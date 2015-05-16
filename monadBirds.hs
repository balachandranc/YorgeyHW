type Bird = Int
type Pole = (Bird,Bird)

landLeft :: Int -> Pole -> Maybe Pole
landLeft n (l, r)
    |   abs ( l + n - r ) < 4   = Just ( l+n, r )
    |   otherwise               = Nothing

landRight :: Int -> Pole -> Maybe Pole
landRight n (l, r)
    |   abs( l - r - n ) < 4    = Just ( l, r+n )
    |   otherwise               = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

-- main = do
--     let result = ( return (0,0) >>= landLeft 1 >>= landRight 1 >>= banana >>= landLeft 3 )
--     putStrLn $ show result

compose = do
    x <- return( 0, 0 )
    y <- landLeft 1 x
    return y    

main = do
    putStrLn $ show compose