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