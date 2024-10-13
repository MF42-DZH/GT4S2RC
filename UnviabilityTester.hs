{-# LANGUAGE OverloadedStrings #-}

module UnviabilityTester where

import Data.Char
import Data.Foldable
import Data.IORef
import SpecIIData hiding ( main )
import ViabilityTester hiding ( main )
import System.IO

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Unviability Brute-Forcer"
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
    [ worker "Searcher #1" (<) currentMaxViability1 divByMissing necessities d1 sp2Data
    , worker "Searcher #2" (<) currentMaxViability2 divByMissing necessities d2 sp2Data
    , worker "Searcher #3" (<) currentMaxViability3 divByMissing necessities d3 sp2Data
    , worker "Searcher #4" (<) currentMaxViability4 divByMissing necessities d4 sp2Data
    , worker "Searcher #5" (<) currentMaxViability5 divByMissing necessities d5 sp2Data
    , worker "Searcher #6" (<) currentMaxViability6 divByMissing necessities d6 sp2Data
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
