{-# LANGUAGE OverloadedStrings #-}

module PermutationTester where

import Data.Char
import Data.Foldable
import Data.IORef
import SpecIIData hiding ( main )
import ViabilityTester hiding ( main )
import System.IO

permutations :: String -> [String]
permutations []       = pure []
permutations (x : xs) =
  [ h : t
  | t <- permutations xs
  , h <- if isAlpha x then [toLower x, toUpper x] else [x]
  ]

split :: [a] -> ([a], [a])
split = go False
  where go _ []           = ([], [])
        go False (x : xs) = let (l, r) = go True xs in (x : l, r)
        go True (x : xs) = let (l, r)  = go False xs in (l, x : r)

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Capitalisation Permutation Brute-Forcer"
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

  let ~(p, u : us) = break isAlpha username
      (d1, d2)     = split $ fmap ((p ++) . (toLower u :)) (permutations us)
      (d3, d4)     = split $ fmap ((p ++) . (toUpper u :)) (permutations us)

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
