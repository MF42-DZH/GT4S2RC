{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char
import Data.Foldable
import Data.IORef
import System.IO
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.S2Data

permutations :: String -> [String]
permutations []       = pure []
permutations (x : xs) =
  [ h : t
  | t <- permutations xs
  , h <- if isAlpha x then [toLower x, toUpper x] else [x]
  ]

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.07 Prize Car Randomizer Capitalisation Permutation Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  sp2Data@(_, _, necessities) <- loadData

  currentMaxViability1 <- newIORef ("", 0, 0)
  currentMaxViability2 <- newIORef ("", 0, 0)
  currentMaxViability3 <- newIORef ("", 0, 0)
  currentMaxViability4 <- newIORef ("", 0, 0)

  putStr "Enter a username with at least one letter: " >> hFlush stdout
  username <- fmap toLower <$> getLine

  putStr "Divide by missing cars for 100% (Y/N): " >> hFlush stdout
  divByMissing <- (== "y") . fmap toLower <$> getLine

  let (p, u : us) = break isAlpha username
      (d1, d2)    = split $ fmap ((p ++) . (toLower u :)) (permutations us)
      (d3, d4)    = split $ fmap ((p ++) . (toUpper u :)) (permutations us)

  sequence
    [ viabilityWorker (WorkerInfo "Searcher #1" (>) currentMaxViability1 sp2Data necessities (viabilityParams divByMissing True d1))
    , viabilityWorker (WorkerInfo "Searcher #2" (>) currentMaxViability2 sp2Data necessities (viabilityParams divByMissing True d2))
    , viabilityWorker (WorkerInfo "Searcher #3" (>) currentMaxViability3 sp2Data necessities (viabilityParams divByMissing True d3))
    , viabilityWorker (WorkerInfo "Searcher #4" (>) currentMaxViability4 sp2Data necessities (viabilityParams divByMissing True d4))
    ] >>= mapM_ joinHandle_

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    , currentMaxViability3
    , currentMaxViability4
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a, _) (_, b, _) -> compare a b) results
