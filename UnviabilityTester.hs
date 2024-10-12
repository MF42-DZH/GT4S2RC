{-# LANGUAGE OverloadedStrings #-}

module UnviabilityTester where

import Data.Foldable
import Data.IORef
import qualified Data.Text.IO as TI
import SpecIIData hiding ( main )
import ViabilityTester hiding ( main )
import System.IO

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Unviability Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  sp2Data <- loadData

  currentMaxViability1 <- newIORef ("", 100000)
  currentMaxViability2 <- newIORef ("", 100000)
  currentMaxViability3 <- newIORef ("", 100000)
  currentMaxViability4 <- newIORef ("", 100000)
  currentMaxViability5 <- newIORef ("", 100000)
  currentMaxViability6 <- newIORef ("", 100000)

  putStr "Enter a max username length: " >> hFlush stdout
  maxLen :: Int <- read <$> getLine

  let (d1, d2, d3, d4, d5, d6) = generateDatasets maxLen

  sequence
    [ worker "Searcher #1" (<) currentMaxViability1 d1 sp2Data
    , worker "Searcher #2" (<) currentMaxViability2 d2 sp2Data
    , worker "Searcher #3" (<) currentMaxViability3 d3 sp2Data
    , worker "Searcher #4" (<) currentMaxViability4 d4 sp2Data
    , worker "Searcher #5" (<) currentMaxViability5 d5 sp2Data
    , worker "Searcher #6" (<) currentMaxViability6 d6 sp2Data
    ] >>= mapM_ joinHandle

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    , currentMaxViability3
    , currentMaxViability4
    , currentMaxViability5
    , currentMaxViability6
    ]

  TI.putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a) (_, b) -> compare b a) results
