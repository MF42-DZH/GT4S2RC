{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module ViabilityTester where

import Control.Concurrent
import Control.Monad.ST
import Control.Monad
import Data.Char
import Data.Foldable
import Data.IORef
import Data.Set ( Set )
import qualified Data.Set as S
import GenerateCombinedList hiding ( main )
import GHC.Exception
import SpecIIData hiding ( main )
import System.IO

type Evaluation = (Username, Double, Int)
type Cmp p      = p -> p -> Bool
newtype JoinHandle a = JH (ThreadId, MVar a)

-- WARNING: In GHC, holding onto the ThreadID after a thread has finished
--          running causes a memory leak.
forkJoinable :: IO a -> IO (JoinHandle a)
forkJoinable action = do
  lock <- newEmptyMVar :: IO (MVar a)
  tid  <- forkFinally action (\ case
    Right x -> putMVar lock x
    Left e  -> throw e)

  return $ JH (tid, lock)

joinHandle :: JoinHandle a -> IO a
joinHandle (JH (_, lock)) = takeMVar lock

joinHandle_ :: JoinHandle a -> IO ()
joinHandle_ = void . joinHandle

joinTid :: JoinHandle a -> ThreadId
joinTid (JH (tid, _)) = tid

charset :: String
charset = ['A'..'Z'] ++ ['a'..'z'] ++ " 0123456789`'\\;,.[]/-=!@#$^&*()~|:<>?_+{}"

determineViability :: Bool -> Necessities -> Username -> SP2Data -> Evaluation
determineViability dbm ns username sp2Data =
  let infoF   = bruteForce username sp2Data (const id)
      vs      = fmap viability infoF
      penalty = 1 + S.size (missingFor100 infoF ns)
  in  (username, fromIntegral (sum vs) / (fromIntegral (length vs) * fromIntegral penalty), penalty - 1)

formatWinner :: String -> Evaluation -> String
formatWinner tn (u, v, m) = concat ["[", tn, "]: New best for criteria ", show v, " from username \"", u, "\", missing ", show m, " cars for 100%."]

isWinner :: Cmp Double -> IORef Evaluation -> Evaluation -> IO (Maybe Evaluation)
isWinner p mx c@(_, v, _) = do
  (_, v', _) <- readIORef mx
  return $ if p v v' then Just c else Nothing

determine :: Cmp Double -> IORef Evaluation -> Bool -> Necessities -> Username -> SP2Data -> IO (Maybe Evaluation)
determine p mx dbm ns un sp2Data =
  let vd = determineViability dbm ns un sp2Data
  in  do
    w <- isWinner p mx vd
    forM_ w (writeIORef mx)
    return w

worker :: String -> Cmp Double -> IORef Evaluation -> Bool -> Necessities -> [Username] -> SP2Data -> IO (JoinHandle ())
worker name p mx dbm ns uns sp2Data = forkJoinable $ forM_ uns $ \ un -> do
  result <- determine p mx dbm ns un sp2Data
  forM_ result $ \ vs -> putStrLn (formatWinner name vs)

generateDatasets :: Int -> ([String], [String], [String], [String], [String], [String])
generateDatasets n
  | n <= 1    = ( fmap pure ['A'..'M']
                , fmap pure ['N'..'Z']
                , fmap pure ['a'..'m']
                , fmap pure ['n'..'z']
                , fmap pure " 0123456789`'\\;,.[]/"
                , fmap pure "-=!@#$^&*()~|:<>?_+{}"
                )
  | otherwise = let (d1, d2, d3, d4, d5, d6) = generateDatasets (n - 1)
                in  ( d1 ++ ((:) <$> charset <*> d1)
                    , d2 ++ ((:) <$> charset <*> d2)
                    , d3 ++ ((:) <$> charset <*> d3)
                    , d4 ++ ((:) <$> charset <*> d4)
                    , d5 ++ ((:) <$> charset <*> d5)
                    , d6 ++ ((:) <$> charset <*> d6)
                    )

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Viability Brute-Forcer"
  putStrLn "Viability value per car provided by TeaKanji\n"

  sp2Data@(_, _, necessities) <- loadData

  currentMaxViability1 <- newIORef ("", 0, 0)
  currentMaxViability2 <- newIORef ("", 0, 0)
  currentMaxViability3 <- newIORef ("", 0, 0)
  currentMaxViability4 <- newIORef ("", 0, 0)
  currentMaxViability5 <- newIORef ("", 0, 0)
  currentMaxViability6 <- newIORef ("", 0, 0)

  putStr "Enter a max username length: " >> hFlush stdout
  maxLen :: Int <- read <$> getLine

  putStr "Divide by missing cars for 100% (Y/N): " >> hFlush stdout
  divByMissing <- (== "y") . fmap toLower <$> getLine

  let (d1, d2, d3, d4, d5, d6) = generateDatasets maxLen

  sequence
    [ worker "Searcher #1" (>) currentMaxViability1 divByMissing necessities d1 sp2Data
    , worker "Searcher #2" (>) currentMaxViability2 divByMissing necessities d2 sp2Data
    , worker "Searcher #3" (>) currentMaxViability3 divByMissing necessities d3 sp2Data
    , worker "Searcher #4" (>) currentMaxViability4 divByMissing necessities d4 sp2Data
    , worker "Searcher #5" (>) currentMaxViability5 divByMissing necessities d5 sp2Data
    , worker "Searcher #6" (>) currentMaxViability6 divByMissing necessities d6 sp2Data
    ] >>= mapM_ joinHandle

  results <- traverse readIORef
    [ currentMaxViability1
    , currentMaxViability2
    , currentMaxViability3
    , currentMaxViability4
    , currentMaxViability5
    , currentMaxViability6
    ]

  putStrLn $ formatWinner "Overall" $ maximumBy (\ (_, a, _) (_, b, _) -> compare a b) results
