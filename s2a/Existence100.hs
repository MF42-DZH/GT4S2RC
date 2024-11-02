{-# LANGUAGE LambdaCase, MultiWayIf, ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.ST
import Data.Word
import Data.Array
import Data.Array.ST
import S2RA.Bruteforce
import S2RA.Concurrent
import S2RA.CRCPRNG ( randInt )
import S2RA.S2Data
import S2RA.Typedefs

worker :: String -> TBQueue Word32 -> TVar Bool -> S2Data -> IO (JoinHandle ())
worker name queue continue ((carCount, cars), events, necessities) = forkJoinable $ do
  let analysisLoop = do
        !mh <- atomically (tryReadTBQueue queue)

        forM_ mh $ \ !hash -> do
          let result = runST (comp hash)
          if | result == 0 -> putStrLn (concat ["\x07[", name, "]: *** 100% SEED FOUND FOR HASH ", show hash, "! ***"])
             | result <= 5 -> putStrLn (concat ["\x07[", name, "]: ", show result, " missing car seed found for hash ", show hash, "."])
             | otherwise   -> pure ()

        atomically (zipF (readTVar continue) (not <$> isEmptyTBQueue queue)) >>= (`when` analysisLoop) . uncurry (||)

  analysisLoop
  where
    !eventFuncFs = fmap (\ (Event g f _) -> fnv1aPrefixF (g ++ f)) events

    comp :: forall s . Word32 -> ST s Int
    comp !initialHash = do
      !missing :: STUArray s Int Bool <- newArray (bounds necessities) True

      forM_ eventFuncFs $ \ ef ->
        let !car = cars ! randInt (ef initialHash) 0 carCount
        in  forM_ (necessaryFor car) $ \ !nf ->
              writeArray missing nf False

      let (minIx, maxIx) = bounds necessities
          go !n !acc
            | n >= minIx && n <= maxIx = readArray missing n >>= \ case
              True  -> go (n + 1) (acc + 1)
              False -> go (n + 1) acc
            | otherwise                = return acc

      go minIx 0

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.07 Prize Car Randomizer 100% Existence Tester"

  sp2Data <- loadData

  shouldContinue <- newTVarIO True

  d1 <- newTBQueueIO @Word32 64
  d2 <- newTBQueueIO @Word32 64
  d3 <- newTBQueueIO @Word32 64
  d4 <- newTBQueueIO @Word32 64
  d5 <- newTBQueueIO @Word32 64

  sequence
    [ worker "Searcher #1" d1 shouldContinue sp2Data
    , worker "Searcher #2" d2 shouldContinue sp2Data
    , worker "Searcher #3" d3 shouldContinue sp2Data
    , worker "Searcher #4" d4 shouldContinue sp2Data
    , worker "Searcher #5" d5 shouldContinue sp2Data
    ] >>= \ ts -> do
      let dataQueues    = [d1, d2, d3, d4, d5]
          lenDataQueues = length dataQueues

          dataLoop :: Word32 -> Int-> IO ()
          dataLoop value pos
            | value == maxBound = atomically (writeTBQueue (dataQueues !! pos) value)
            | otherwise         = atomically (writeTBQueue (dataQueues !! pos) value)
              >> dataLoop (value + 1) ((pos + 1) `mod` lenDataQueues)

      forkJoinable (dataLoop minBound 0 >> writeIO shouldContinue False) >>= joinHandle_
      mapM_ joinHandle_ ts

  putStrLn "\n\x07----- Finished. -----"

