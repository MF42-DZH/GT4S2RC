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

split :: [a] -> ([a], [a])
split = go False
  where go _ []           = ([], [])
        go False (x : xs) = let (l, r) = go True xs in (x : l, r)
        go True (x : xs) = let (l, r)  = go False xs in (l, x : r)

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Capitalisation LSB Bitflip Brute-Forcer"
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
