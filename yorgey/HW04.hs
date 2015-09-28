{-# OPTIONS_GHC -Wall #-}
module HW04 where
import Data.List (intersperse)

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [ 0, 1 ]

-- Exercise 2 ----------------------------------------

canonical :: ( Num a, Eq a ) => [a] -> [a]
canonical arr = reverse $ dropWhile ((==) 0) $ reverse arr

instance (Num a, Eq a) => Eq (Poly a) where
    (==) (P l1) (P l2) = ( canonical l1 ) == ( canonical l2 )
 
-- Exercise 3 -----------------------------------------

prefix 1 = ""
prefix (-1) = "-"
prefix p = show p

suffix 1 = ""
suffix ord = "^" ++ show ord

term ( coeff, 0 ) = show coeff
term ( coeff, ord ) = ( prefix coeff ) ++ "x" ++ ( suffix ord )

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P [0]) = "0"
    show (P xs) = let termList = map term $ filter ( (/=) 0 . fst ) $ zip ( canonical xs ) [0..]
                    in concat $ reverse $ intersperse " + " $ termList

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus = undefined

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times = undefined

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate      = undefined
    fromInteger = undefined
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP = undefined

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv = undefined

-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv = undefined
