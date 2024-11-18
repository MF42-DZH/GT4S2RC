{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Foldable
import Data.IORef
import Data.List ( nub )
import System.IO
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.S2Data

flips :: Bool -> String -> [String]
flips _ []           = pure []
flips permC (x : xs) =
  [ h : t
  | t <- flips permC xs
  , h <- let cs = [x, chr (ord x .^. 1)]
         in  if permC then extend cs else cs
  ]

extend :: [Char] -> [Char]
extend [a, b] = nub [toLower a, toUpper a, toLower b, toUpper b]
extend xs     = nub xs

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.08 Prize Car Randomizer Capitalisation LSB Bitflip Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  sp2Data@(_, _, necessities) <- loadData

  currentMaxViability1 <- newIORef ("", 0, 0)
  currentMaxViability2 <- newIORef ("", 0, 0)
  currentMaxViability3 <- newIORef ("", 0, 0)
  currentMaxViability4 <- newIORef ("", 0, 0)

  putStr "Enter a username: " >> hFlush stdout
  username <- getLine

  putStr "Also permute capitalisation (Y/N): " >> hFlush stdout
  permC <- (== "y") . fmap toLower <$> getLine

  putStr "Divide by missing cars for 100% (Y/N): " >> hFlush stdout
  divByMissing <- (== "y") . fmap toLower <$> getLine

  let ((d1, d2), (d3, d4)) = bimap split split $ split (flips permC username)

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
