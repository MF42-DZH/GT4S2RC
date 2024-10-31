{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Foldable
import Data.IORef
import System.IO
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.S2Data

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.07 Prize Car Randomizer Unviability Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  sp2Data@(_, _, necessities) <- loadData

  currentMaxViability1 <- newIORef ("", 100000, 0)
  currentMaxViability2 <- newIORef ("", 100000, 0)
  currentMaxViability3 <- newIORef ("", 100000, 0)
  currentMaxViability4 <- newIORef ("", 100000, 0)
  currentMaxViability5 <- newIORef ("", 100000, 0)
  currentMaxViability6 <- newIORef ("", 100000, 0)

  putStr "Enter a max username length: " >> hFlush stdout
  maxLen :: Int <- read <$> getLine

  putStr "Divide by missing cars for 100% (Y/N): " >> hFlush stdout
  divByMissing <- (== "y") . fmap toLower <$> getLine

  let (d1, d2, d3, d4, d5, d6) = generateDatasets maxLen

  sequence
    [ viabilityWorker (WorkerInfo "Searcher #1" (<) currentMaxViability1 sp2Data necessities (viabilityParams divByMissing True d1))
    , viabilityWorker (WorkerInfo "Searcher #2" (<) currentMaxViability2 sp2Data necessities (viabilityParams divByMissing True d2))
    , viabilityWorker (WorkerInfo "Searcher #3" (<) currentMaxViability3 sp2Data necessities (viabilityParams divByMissing True d3))
    , viabilityWorker (WorkerInfo "Searcher #4" (<) currentMaxViability4 sp2Data necessities (viabilityParams divByMissing True d4))
    , viabilityWorker (WorkerInfo "Searcher #5" (<) currentMaxViability5 sp2Data necessities (viabilityParams divByMissing True d5))
    , viabilityWorker (WorkerInfo "Searcher #6" (<) currentMaxViability6 sp2Data necessities (viabilityParams divByMissing True d6))
    ] >>= mapM_ joinHandle

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    , currentMaxViability3
    , currentMaxViability4
    , currentMaxViability5
    , currentMaxViability6
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a, _) (_, b, _) -> compare b a) results
