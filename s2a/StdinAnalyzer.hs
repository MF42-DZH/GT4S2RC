{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import System.Environment
import System.IO
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.S2Data

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.09 Prize Car Randomizer Viability Stdin Analyzer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  args <- getArgs
  let divByMissing = foldr (\ x -> (((fmap toLower x == "-m") || (fmap toLower x == "--missing")) ||)) False args

  when divByMissing $ putStrLn "Dividing by missing cars for 100%.\n"

  sp2Data@(_, _, necessities) <- loadData

  currentMaxViability1 <- newIORef ("", 0, 0)
  currentMaxViability2 <- newIORef ("", 0, 0)
  currentMaxViability3 <- newIORef ("", 0, 0)
  currentMaxViability4 <- newIORef ("", 0, 0)

  shouldContinue <- newMVar True

  d1 <- newTBQueueIO @Username 5
  d2 <- newTBQueueIO @Username 5
  d3 <- newTBQueueIO @Username 5
  d4 <- newTBQueueIO @Username 5

  sequence
    [ viabilityWorkerSTM (WorkerInfo "Searcher #1" (>) currentMaxViability1 sp2Data necessities (viabilityParams divByMissing shouldContinue d1))
    , viabilityWorkerSTM (WorkerInfo "Searcher #2" (>) currentMaxViability2 sp2Data necessities (viabilityParams divByMissing shouldContinue d2))
    , viabilityWorkerSTM (WorkerInfo "Searcher #3" (>) currentMaxViability3 sp2Data necessities (viabilityParams divByMissing shouldContinue d3))
    , viabilityWorkerSTM (WorkerInfo "Searcher #4" (>) currentMaxViability4 sp2Data necessities (viabilityParams divByMissing shouldContinue d4))
    ] >>= \ ts -> do
      let readLoop :: Int -> IO ()
          readLoop 1 = isEOF >>= (`unless` ((*> readLoop 2) $! (getLine >>= atomically . writeTBQueue d1)))
          readLoop 2 = isEOF >>= (`unless` ((*> readLoop 3) $! (getLine >>= atomically . writeTBQueue d2)))
          readLoop 3 = isEOF >>= (`unless` ((*> readLoop 4) $! (getLine >>= atomically . writeTBQueue d3)))
          readLoop 4 = isEOF >>= (`unless` ((*> readLoop 1) $! (getLine >>= atomically . writeTBQueue d4)))

      forkJoinable (readLoop 1 *> modifyMVar_ shouldContinue (pure . const False)) >>= joinHandle_
      mapM_ joinHandle_ ts

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    , currentMaxViability3
    , currentMaxViability4
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a, _) (_, b, _) -> compare a b) results
