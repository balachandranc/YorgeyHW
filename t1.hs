doubleMe x = x + x

mylast [x] = x
mylast (x:xs) = mylast xs

itemAt :: [a] -> Int -> Maybe a
itemAt (x:xs) 0 = Just x
itemAt (x:xs) n = itemAt xs ( n - 1 )
itemAt [] n = Nothing

mdrop xs 0 = xs
mdrop xs n = if n > length xs
                then []
                else mdrop ( tail xs ) ( n - 1 )

mcycle (xs) = xs : mcycle xs

csize width height
    |   area < 100 = "small rect"
    |   otherwise  = "large rect"
    where area = width * height

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [ bmi w h | (w,h) <- xs ]
    where bmi width height = width * height + zero
            where zero = 0

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r^2
    in  sideArea + 2 * topArea


fbfilter :: Int -> [Char]
fbfilter x
    |   divBy15 x   = "fizzbuzz"
    |   divBy3 x    = "fizz"
    |   divBy5 x    = "buzz"
    |   otherwise   = show x
    where   divBy5 n = ( mod n 5 ) == 0
            divBy3 n = ( mod n 3 ) == 0
            divBy15 n = ( mod n 15 ) == 0

describeList :: [a] -> [Char]
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "singleton."
                                               xs -> "long." 


tt :: [Char] -> String
tt x = x

mmax :: (Ord a) => [a] -> Maybe a
mmax [] = Nothing
mmax [x] = Just x
-- mmax (x:xs) = foldr (\x y-> if x > y then x else y ) x xs
mmax (x:xs)
    | x > maxTail   = Just x
    | otherwise     = Just maxTail
    where Just maxTail = mmax xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [ a | a <- xs, a <= x ]
        biggerSorted  = quicksort [ a | a <- xs, a > x ]
    in smallerSorted ++ [ x ] ++ biggerSorted

numUniques :: (Eq a) => [a] -> Int
numUniques [] = 0
numUniques x = 
    let uniques = foldr ( \x acc-> if elem x acc then acc else x:acc ) []
    in  length $ uniques x


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose x
    | maxLength x > 1 = heads x : transpose ( filter ( not . null ) ( map tail x ) )
    | otherwise = [ heads x ]
    where   heads x = map head x
            maxLength x = foldl max 0 ( map length x )

mconcat :: [[a]] -> [a]
mconcat [[x]] = [x]
mconcat ([x]:xs) = x : mconcat xs
mconcat ((x:xs):y) = x : mconcat (xs:y)

miterate :: (a -> a) -> a -> [a]
miterate f v = v : miterate f ( f v )

mgroup :: (Eq a) => [a] -> [[a]]
mgroup []  = []
mgroup (x:xs) = y : mgroup ys
    where (y,ys) = span (==x) (x:xs)

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

area :: Shape -> Float
area ( Circle x y r ) = 3.14 * r * r
area ( Rectangle x y w h ) = w * h
