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

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Capitalisation Permutation Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  sp2Data <- loadData

  currentMaxViability1 <- newIORef ("", 0)
  currentMaxViability2 <- newIORef ("", 0)

  putStr "Enter a username with at least one letter: " >> hFlush stdout
  username <- fmap toLower <$> getLine

  let ~(p, u : us) = break isAlpha username
      d1           = fmap ((p ++) . (toLower u :)) (permutations us)
      d2           = fmap ((p ++) . (toUpper u :)) (permutations us)

  sequence
    [ worker "Searcher #1" (>) currentMaxViability1 d1 sp2Data
    , worker "Searcher #2" (>) currentMaxViability2 d2 sp2Data
    ] >>= mapM_ joinHandle_

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a) (_, b) -> compare a b) results
