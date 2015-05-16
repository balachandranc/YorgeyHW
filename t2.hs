import Data.Char

main = interact $ unlines . map palindrome . lines

shortLinesOnly :: [Char] -> [Char]
shortLinesOnly x = 
    let l = lines x
        filtered = filter (\x -> length x < 10) l
    in unlines filtered

palindrome :: [Char] -> [Char]
palindrome str 
    |   strLen < 2 = "palindrome"
    |   str !! 0 /= str !! (strLen - 1) = "not a palindrome"
    |   otherwise = palindrome $ take ( strLen - 2 ) ( tail str )
    where strLen = length str