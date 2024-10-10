{-# LANGUAGE LambdaCase #-}

module ViabilityTester where

import Control.Concurrent
import Control.Monad.ST
import Control.Monad
import Data.Foldable
import Data.IORef
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
formatWinner tn (u, v) = concat ["[", tn, "]: New local maximum viability ", show v, " from username ", show u, "."]

isWinner :: IORef (String, Double) -> (String, Double) -> IO (Maybe (String, Double))
isWinner mx c@(_, v) = do
  (_, v') <- readIORef mx
  return $ if v > v' then Just c else Nothing

determine :: IORef (String, Double) -> String -> CarInfo -> [String] -> [String] -> IO (Maybe (String, Double))
determine mx un cs rs fs =
  let vd = determineViability un cs rs fs
  in  do
    w <- isWinner mx vd
    forM_ w (writeIORef mx)
    return w

worker :: String -> IORef (String, Double) -> [String] -> CarInfo -> [String] -> [String] -> IO (JoinHandle ())
worker name mx uns cs rs fs = forkJoinable $ forM_ uns $ \ un -> do
  result <- determine mx un cs rs fs
  forM_ result $ \ vs -> putStrLn (formatWinner name vs)

generateDatasets :: Int -> ([String], [String], [String], [String], [String], [String])
generateDatasets n
  | n <= 1    = ( fmap pure ['A'..'O']
                , fmap pure ['P'..'Z']
                , fmap pure ['a'..'o']
                , fmap pure ['p'..'z']
                , fmap pure " 0123456789`'\\;,.[]/"
                , fmap pure "-=!@#$^&*()~|:<>?_+{}"
                )
  | otherwise = let (d1, d2, d3, d4, d5, d6) = generateDatasets (n - 1)
                in  ( d1 ++ ((:) <$> charset <*> d1)
                    , d2 ++ ((:) <$> charset <*> d2)
                    , d3 ++ ((:) <$> charset <*> d3)
                    , d4 ++ ((:) <$> charset <*> d4)
                    , d5 ++ ((:) <$> charset <*> d5)
                    , d6 ++ ((:) <$> charset <*> d6)
                    )

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Viability Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  cars  <- loadCars
  races <- lines <$> readFile "RACELIST.txt"
  funcs <- lines <$> readFile "FUNCLIST.txt"

  currentMaxViability1 <- newIORef ("", 0)
  currentMaxViability2 <- newIORef ("", 0)
  currentMaxViability3 <- newIORef ("", 0)
  currentMaxViability4 <- newIORef ("", 0)
  currentMaxViability5 <- newIORef ("", 0)
  currentMaxViability6 <- newIORef ("", 0)

  putStr "Enter a max username length: " >> hFlush stdout
  maxLen :: Int <- read <$> getLine

  let (d1, d2, d3, d4, d5, d6) = generateDatasets maxLen

  sequence
    [ worker "Searcher #1" currentMaxViability1 d1 cars races funcs
    , worker "Searcher #2" currentMaxViability2 d2 cars races funcs
    , worker "Searcher #3" currentMaxViability3 d3 cars races funcs
    , worker "Searcher #4" currentMaxViability4 d4 cars races funcs
    , worker "Searcher #5" currentMaxViability5 d5 cars races funcs
    , worker "Searcher #6" currentMaxViability6 d6 cars races funcs
    ] >>= mapM_ joinHandle

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    , currentMaxViability3
    , currentMaxViability4
    , currentMaxViability5
    , currentMaxViability6
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a) (_, b) -> compare a b) results
