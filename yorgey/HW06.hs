{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 ( tail fibs2 )

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 20 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList ( Cons x y ) = x : streamToList y 

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f ( Cons x rest ) = Cons ( f x ) ( fmap f rest )

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat val = Cons val ( sRepeat val )

sIterate :: (a -> a) -> a -> Stream a
sIterate rule seed = Cons seed ( sIterate rule $ rule seed )

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x s1) s2 = Cons x ( sInterleave s2 s1 )

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n ( Cons x rest ) = x : sTake ( n - 1 ) rest

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (1+) 0

ruler :: Stream Integer
ruler = sInterleave ( sRepeat 0 ) ( fmap (+1) ruler )

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = sIterate (\x -> ( 1103515245 * x + 12345 ) `mod` 2147483648 ) seed

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 237 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax xs = Just $ foldl' ( \( !mn, !mx ) x -> ( min mn x, max mx x ) ) (xs !! 0, xs !! 0) xs 

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = Matrix [ Integer ]
                deriving (Show)

mF :: Matrix
mF = Matrix [ 1, 1, 1, 0 ]

instance Num Matrix where
    Matrix xs * Matrix ys = Matrix [    ( (xs!!0) * (ys!!0) + (xs!!1) * (ys!!2) ),
                                        ( (xs!!0) * (ys!!1) + (xs!!1) * (ys!!3) ),
                                        ( (xs!!2) * (ys!!0) + (xs!!3) * (ys!!2) ),
                                        ( (xs!!2) * (ys!!1) + (xs!!3) * (ys!!3) )] 

fastFib :: Int -> Integer
fastFib 0 = 1
fastFib n = projectFib $ mF ^ n
    where projectFib ( Matrix xs ) = xs !! 1
