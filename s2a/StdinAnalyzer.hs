module Main where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.IORef
import Data.List
import System.Environment
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.S2Data

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Viability Stdin Analyzer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  args <- getArgs
  let divByMissing = foldr (\ x -> (((fmap toLower x == "-m") || (fmap toLower x == "--missing")) ||)) False args

  when divByMissing $ putStrLn "Dividing by missing cars for 100%.\n"

  sp2Data@(_, _, necessities) <- loadData

  currentMaxViability1 <- newIORef ("", 0, 0)
  currentMaxViability2 <- newIORef ("", 0, 0)
  currentMaxViability3 <- newIORef ("", 0, 0)
  currentMaxViability4 <- newIORef ("", 0, 0)

  ((d1, d2), (d3, d4)) <- bimap split split . split . lines <$> getContents

  sequence
    [ worker "Searcher #1" (>) currentMaxViability1 divByMissing necessities d1 sp2Data
    , worker "Searcher #2" (>) currentMaxViability2 divByMissing necessities d2 sp2Data
    , worker "Searcher #3" (>) currentMaxViability3 divByMissing necessities d3 sp2Data
    , worker "Searcher #4" (>) currentMaxViability4 divByMissing necessities d4 sp2Data
    ] >>= mapM_ joinHandle_

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    , currentMaxViability3
    , currentMaxViability4
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a, _) (_, b, _) -> compare a b) results
