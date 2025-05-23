module Main where

import Data.Char
import Data.IORef
import System.IO
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.S2Data

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.09 Prize Car Randomizer Viability File Analyzer"
  putStrLn "Viability value per car provided by TeaKanji\n"
  putStrLn "Please provide a newline-separated list of usernames to test."

  sp2Data@(_, _, necessities) <- loadData

  currentMaxViability <- newIORef ("", 0, 0)

  putStr "Enter the filepath to analyze: " >> hFlush stdout
  path <- getLine

  names <- lines <$> readFile path

  putStr "Divide by missing cars for 100% (Y/N): " >> hFlush stdout
  divByMissing <- (== "y") . fmap toLower <$> getLine

  viabilityWorker (WorkerInfo "Searcher" (>) currentMaxViability sp2Data necessities (viabilityParams divByMissing True names)) >>= joinHandle_

  result <- readIORef currentMaxViability
  putStrLn $ formatWinner "Overall" result
