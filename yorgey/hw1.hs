lastDigit :: Integer -> Integer
lastDigit x = mod x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = floor( ( fromIntegral x ) / 10 )

toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x < 1   = []
    | x > 9   = ld : toRevDigits( dropLastDigit x )
    | otherwise = [ld]
    where ld = lastDigit x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : 2 * y : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits x = sum $ x >>= toRevDigits

luhn :: Integer -> Bool
luhn x = ( checksum x ) `mod` 10 == 0
    where checksum = sumDigits . doubleEveryOther . toRevDigits


type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src dst tmp = [(src,dst)]
hanoi count src dst tmp = hanoi ( count - 1 ) src tmp dst ++ [(src,dst)] ++ hanoi ( count - 1 ) tmp dst src


-- Lars H stackoverflow: http://stackoverflow.com/questions/3607161/towers-of-hanoi-with-k-pegs
-- answer for 15 pegs doesnt match hw hint

-- hanoi for n disks and r pegs [p1, p2, ..., pr]
hanoiR :: Int -> [a] -> [(a, a)]

-- zero disks: no moves needed.
hanoiR 0 _ = []

-- one disk: one move and two pegs needed.
hanoiR 1 (p1 : p2 : rest) = [(p1, p2)] -- only needed for smart-alecks?

{-
-- n disks and 3 pegs -- unneeded; covered by (null rest) below.
hanoiR n [p1, p2, p3] =
    hanoiR (n - 1) [p1, p3, p2] ++
    [(p1, p2)] ++
    hanoiR (n - 1) [p3, p2, p1]
-}

-- n disks and r > 3 pegs: use Frame-Stewart algorithm
hanoiR n (p1 : p2 : p3 : rest) = hanoiR k (p1 : p3 : p2 : rest) ++ hanoiR (n - k) (p1 : p2 : rest) ++ hanoiR k (p3 : p2 : p1 : rest)
    where k
            | null rest = n - 1
            | otherwise = n `quot` 2
