{-# LANGUAGE LambdaCase #-}

module ViabilityTester where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.ST
import Control.Monad
import Data.Foldable
import GenerateCombinedList hiding ( main )
import GHC.Exception
import SpecIIData hiding ( main )
import System.IO

newtype JoinHandle a = JH (ThreadId, MVar a)

-- WARNING: In GHC, holding onto the ThreadID after a thread has finished
--          running causes a memory leak.
forkJoinable :: IO a -> IO (JoinHandle a)
forkJoinable action = do
  lock <- newEmptyMVar :: IO (MVar a)
  tid  <- forkFinally action (\ case
    Right x -> putMVar lock x
    Left e  -> throw e)

  return $ JH (tid, lock)

joinHandle :: JoinHandle a -> IO a
joinHandle (JH (_, lock)) = takeMVar lock

joinHandle_ :: JoinHandle a -> IO ()
joinHandle_ = void . joinHandle

joinTid :: JoinHandle a -> ThreadId
joinTid (JH (tid, _)) = tid

charset :: String
charset = ['A'..'Z'] ++ ['a'..'z'] ++ " 0123456789`\\;,.[]/-="

determineViability :: String -> CarInfo -> [String] -> [String] -> (String, Double)
determineViability username cars races funcs =
  let vs = bruteForce username cars races funcs (const viability)
  in  (username, fromIntegral (sum vs) / fromIntegral (length vs))

formatWinner :: String -> (String, Double) -> String
formatWinner tn (u, v) = concat ["[", tn, "]: New maximum viability ", show v, " from username ", show u, "."]

isWinner :: TVar (String, Double) -> (String, Double) -> STM (Maybe (String, Double))
isWinner mx c@(_, v) = do
  (_, v') <- readTVar mx
  return $ if v > v' then Just c else Nothing

determine :: TVar (String, Double) -> String -> CarInfo -> [String] -> [String] -> STM (Maybe (String, Double))
determine mx un cs rs fs =
  let vd = determineViability un cs rs fs
  in  do
    w <- isWinner mx vd
    forM_ w (writeTVar mx)
    return w

worker :: String -> TVar (String, Double) -> [String] -> CarInfo -> [String] -> [String] -> IO (JoinHandle ())
worker name mx uns cs rs fs = forkJoinable $ forM_ uns $ \ un -> do
  result <- atomically (determine mx un cs rs fs)
  forM_ result $ \ vs -> putStrLn (formatWinner name vs)


generateDatasets :: Int -> ([String], [String], [String], [String], [String])
generateDatasets n
  | n <= 1    = ( fmap pure ['A'..'O']
                , fmap pure ['P'..'Z']
                , fmap pure ['a'..'o']
                , fmap pure ['p'..'z']
                , fmap pure " 0123456789`\\;,.[]/-="
                )
  | otherwise = let (d1, d2, d3, d4, d5) = generateDatasets (n - 1)
                in  ( d1 ++ ((\ str c -> str ++ [c]) <$> d1 <*> charset)
                    , d2 ++ ((\ str c -> str ++ [c]) <$> d2 <*> charset)
                    , d3 ++ ((\ str c -> str ++ [c]) <$> d3 <*> charset)
                    , d4 ++ ((\ str c -> str ++ [c]) <$> d4 <*> charset)
                    , d5 ++ ((\ str c -> str ++ [c]) <$> d5 <*> charset)
                    )

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Viability Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  cars  <- loadCars
  races <- lines <$> readFile "RACELIST.txt"
  funcs <- lines <$> readFile "FUNCLIST.txt"

  currentMaxViability <- newTVarIO ("", 0)

  putStr "Enter a max username length: " >> hFlush stdout
  maxLen :: Int <- read <$> getLine

  let (d1, d2, d3, d4, d5) = generateDatasets maxLen

  w1 <- worker "Searcher #1" currentMaxViability d1 cars races funcs
  w2 <- worker "Searcher #2" currentMaxViability d2 cars races funcs
  w3 <- worker "Searcher #3" currentMaxViability d3 cars races funcs
  w4 <- worker "Searcher #4" currentMaxViability d4 cars races funcs
  w5 <- worker "Searcher #5" currentMaxViability d5 cars races funcs

  traverse_ joinHandle_ [w1, w2, w3, w4, w5]
