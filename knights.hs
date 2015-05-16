import Control.Monad

type KnightPos = (Int,Int)

dists = [ y x | x <- [1,2], y <- [id, negate] ]
moves = [ (x,y) | x <- dists, y <- dists, abs x /= abs y ]

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [ ( c + dc, r + dr ) | (dc, dr) <- moves ]
    guard ( c' `elem` [1..8] && r' `elem` [1..8] )
    return (c',r')

checkReachable :: KnightPos -> [KnightPos] -> Int -> Bool
checkReachable dstPos srcPoss n
    |   n == 0  = elem dstPos srcPoss
    |   otherwise = checkReachable dstPos [ pos | srcPos <- srcPoss, pos <- moveKnight srcPos ] ( n - 1 )


in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight