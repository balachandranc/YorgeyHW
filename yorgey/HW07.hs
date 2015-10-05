{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V
import qualified Data.List as L


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= (\x -> return $ f x )

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 vec = do
  v1 <- vec !? i1
  v2 <- vec !? i2
  return $ vec // [(i1,v2),(i2,v1)]

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence $ map f xs

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices vec = mapM (\i -> vec !? i) indices

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec = do
  ind <- getRandomR (0, ( V.length vec - 1 ) )
  return $ vec !? ind

-- Exercise 4 -----------------------------------------

randomList :: Random a => Int -> Rnd ([a])
randomList 0 = return []
randomList n = do
  first <- getRandom
  rest <- randomList ( n - 1 )
  return (first:rest)

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = do
  vals <- randomList n
  return $ V.fromList vals

randomListR :: Random a => Int -> (a, a) -> Rnd ([a])
randomListR 0 _ = return []
randomListR n range = do
  first <- getRandomR range
  rest <- randomListR ( n - 1 ) range
  return (first:rest)

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n range = do
  vals <- randomListR n range
  return $ V.fromList vals

-- Exercise 5 -----------------------------------------

swap :: (Int,Int) -> Vector a -> Vector a
swap (i1,i2) vec = vec // [(i1, vec ! i2), (i2, vec ! i1)]

shuffle' :: Vector a -> Rnd ( Vector a )
shuffle' vec = do
  let uppers = reverse [ 1 .. ( ( V.length vec ) - 1 ) ]
  indices <- mapM ( \x -> getRandomR (0,x) ) uppers
  return $ foldl (\v tup -> swap tup v) vec $ zip uppers indices

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = do
  if ( V.length vec ) < 2
    then return vec
    else shuffle' vec

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec index = let   pivot = vec ! index
                              (left, right) = splitAt index $ V.toList vec
                              (smaller, larger) = L.partition (< pivot) $ left ++ tail right
                        in    (V.fromList smaller, pivot, V.fromList larger)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec
      | V.length vec <= 1 = vec
      | otherwise = let ( smaller, pivot, larger ) = partitionAt vec 0
                    in ( qsort smaller ) <> V.cons pivot ( qsort larger )
-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec 
        | V.length vec <= 1 = return vec
        | otherwise = do
          pivotIndex <- getRandomR (0, ( V.length vec - 1 ) )
          let ( smaller, pivot, larger ) = partitionAt vec pivotIndex
          left <- qsortR smaller
          right <- qsortR larger
          return $ left <> V.cons pivot right

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i vec = do
  if ( V.length vec ) == 0
    then do
      return Nothing
    else do
      pivotIndex <- getRandomR (0, ( V.length vec - 1 ))
      let ( smaller, _, larger ) = partitionAt vec pivotIndex
      case compare pivotIndex i of
        EQ -> return $ vec !? pivotIndex
        LT -> select ( i - pivotIndex - 1 ) larger
        GT -> select i smaller


-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card label suit | label <- labels, suit <- suits ]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck = do
  card <- deck !? 0
  let rest = V.tail deck
  return ( card, rest )

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 deck = Just ([], deck)
getCards n deck = do
  ( card, nextDeck ) <- nextCard deck
  ( cards, finalDeck ) <- getCards ( n - 1 ) nextDeck
  return ( card: cards, finalDeck )

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
