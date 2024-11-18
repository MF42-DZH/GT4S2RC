{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Prelude hiding ( readIO )

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.List
import Data.Word
import System.IO hiding ( readIO )
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.S2Data

newtype MissingParams = MissingParams (TVar Bool, TBQueue String)

missingWorkerSTM :: WorkerInfo Int MissingParams -> IO (JoinHandle ())
missingWorkerSTM = baseWorker withStm
  where
    determineM wi username =
      let missing = bruteForceMissingCount username (wiData wi)
      in  do
        w <- isWinner (wiCmp wi) (wiBest wi) (username, missing, missing)
        forM_ w (writeIORef (wiBest wi))
        return w

    withStm wi@(WorkerInfo name _ mx _ _ (MissingParams (shouldContinue, uns))) = do
      haveSearchedRaw <- newTVarIO (0 :: Word64)
      haveSearchedMod <- newTVarIO (0 :: Word64)

      let printer u = do
            atomically $ do
              modifyTVar' haveSearchedRaw (+ 1)
              modifyTVar' haveSearchedMod ((`mod` 200000) . (+ 1))

            atomically (zipF (readTVar haveSearchedMod) (readTVar haveSearchedRaw)) >>= \ (h, r) ->
              when (h == 0) $ do
                putStrLn (concat ["[", name, "]: Searched ", show r, " names. Currently looking at: \"", u, "\"."])
                threadDelay 500000
          analysisLoop = do
            !mu <- atomically (tryReadTBQueue uns)

            forM_ mu $ \ !un -> do
              result <- determineM wi un
              forM_ result $ \ !vs -> putStrLn (formatWinner name vs)
              printer un

            atomically (zipF (readTVar shouldContinue) (not <$> isEmptyTBQueue uns)) >>= (`when` analysisLoop) . uncurry (||)

      analysisLoop

      summarizeSearch name haveSearchedRaw mx

    summarizeSearch name raw mx = zipF (readIO raw) (readIO mx) >>= \ (r, (n, v, m)) -> putStrLn $ concat
      [ "[", name, "]: Searched a total of "
      , show r
      , " names.\n"
      , "[", name, "]: Best result from username \""
      , n
      , "\", missing "
      , show m
      , " cars for 100%."
      ]

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.08 Prize Car Randomizer Viability Stdin Missing Cars Analyzer"

  sp2Data@(_, _, necessities) <- loadData

  currentMissing1 <- newIORef ("", maxBound, 0)
  currentMissing2 <- newIORef ("", maxBound, 0)
  currentMissing3 <- newIORef ("", maxBound, 0)
  currentMissing4 <- newIORef ("", maxBound, 0)
  currentMissing5 <- newIORef ("", maxBound, 0)

  shouldContinue <- newTVarIO True

  d1 <- newTBQueueIO @Username 16
  d2 <- newTBQueueIO @Username 16
  d3 <- newTBQueueIO @Username 16
  d4 <- newTBQueueIO @Username 16
  d5 <- newTBQueueIO @Username 16

  sequence
    [ missingWorkerSTM (WorkerInfo "Searcher #1" (<=) currentMissing1 sp2Data necessities (MissingParams (shouldContinue, d1)))
    , missingWorkerSTM (WorkerInfo "Searcher #2" (<=) currentMissing2 sp2Data necessities (MissingParams (shouldContinue, d2)))
    , missingWorkerSTM (WorkerInfo "Searcher #3" (<=) currentMissing3 sp2Data necessities (MissingParams (shouldContinue, d3)))
    , missingWorkerSTM (WorkerInfo "Searcher #4" (<=) currentMissing4 sp2Data necessities (MissingParams (shouldContinue, d4)))
    , missingWorkerSTM (WorkerInfo "Searcher #5" (<=) currentMissing5 sp2Data necessities (MissingParams (shouldContinue, d5)))
    ] >>= \ ts -> do
      let readLoop :: Int -> IO ()
          readLoop 1 = isEOF >>= (`unless` ((*> readLoop 2) $! (getLine >>= atomically . writeTBQueue d1)))
          readLoop 2 = isEOF >>= (`unless` ((*> readLoop 3) $! (getLine >>= atomically . writeTBQueue d2)))
          readLoop 3 = isEOF >>= (`unless` ((*> readLoop 4) $! (getLine >>= atomically . writeTBQueue d3)))
          readLoop 4 = isEOF >>= (`unless` ((*> readLoop 5) $! (getLine >>= atomically . writeTBQueue d4)))
          readLoop 5 = isEOF >>= (`unless` ((*> readLoop 1) $! (getLine >>= atomically . writeTBQueue d5)))

      forkJoinable (readLoop 1 *> writeIO shouldContinue False) >>= joinHandle_
      mapM_ joinHandle_ ts

  results <- traverse readIORef
    [ currentMissing1
    , currentMissing2
    , currentMissing3
    , currentMissing4
    , currentMissing5
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a, _) (_, b, _) -> compare a b) results
