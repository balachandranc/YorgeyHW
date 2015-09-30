{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

import Data.Bits (xor)
import Data.List (sortBy)

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret mFile oFile = do
  modified <- BS.readFile mFile
  original <- BS.readFile oFile
  return $ BS.pack $ filter (/=0) $ BS.zipWith xor modified original

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey secret outFilename = do
  encrypted <- BS.readFile $ outFilename ++ ".enc"
  let decrypted = BS.pack $ BS.zipWith xor ( BS.cycle secret ) encrypted
  BS.writeFile outFilename decrypted

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filename = do
  contents <- BS.readFile filename
  return $ decode contents

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimList transactionData = do
  (Just victimTids) <- parseFile victimList ::  IO (Maybe [TId])
  (Just transactions) <- parseFile transactionData :: IO (Maybe [Transaction])
  return $ Just $ filter (\x -> ( tid x ) `elem` victimTids ) transactions 

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = let  tuples = concatMap (\t -> [( from t, -(amount t)), ( to t, (amount t ) )]) transactions
                            in foldr ( \(k,v) -> Map.insertWith (+) k v ) Map.empty tuples

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal candidateMap = fst $ foldr (\(name1, value1) (name2,value2) -> if value1 > value2 then (name1,value1) else (name2,value2)) ("", toInteger (minBound::Int )) $ Map.toList candidateMap

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs candidateMap tids = let  ( payers, payees ) =  Map.partition (>0) candidateMap
                                payerList = sortByValue payers
                                payeeList = sortByValue $ fmap abs payees
                              in createTransactions payerList payeeList tids
                              where   sortByValue cmap = sortBy ( \(_,v1) (_,v2) -> compare v2 v1 ) $ Map.toList cmap
                                      createTransactions [] _ _ = []
                                      createTransactions _ [] _ = []
                                      createTransactions _ _ [] = []
                                      createTransactions ((prName,prValue):payers) ((peName,peValue):payees) (t:tids')
                                        | prValue > peValue = ( Transaction { from = prName, to = peName, amount = peValue, tid = t } ) : createTransactions ((prName,prValue-peValue):payers) payees tids'
                                        | prValue < peValue = ( Transaction { from = prName, to = peName, amount = prValue, tid = t } ) : createTransactions payers ((peName,peValue-prValue):payees) tids'
                                        | prValue == peValue= ( Transaction { from = prName, to = peName, amount = prValue, tid = t } ) : createTransactions payers payees tids' 
-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON outFilename transactions = BS.writeFile outFilename $ encode transactions

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

