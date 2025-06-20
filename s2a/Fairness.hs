{-# LANGUAGE OverloadedStrings #-}

module Main where

-- This has two modes of operation: run on its own, or run as a "simple" output mode for parsing in
-- other programs. In "simple" output mode, stdout will continually output 2-byte integers, and
-- output 0xFFFF when finished.

import Control.Monad
import Data.Array
import Data.Array.MArray
import Data.Array.IO
import Data.Binary
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Word
import System.Environment
import System.IO
import S2RA.S2Data
import S2RA.Typedefs

runSimple :: IO ()
runSimple = do
  (cars, events, _) <- loadData

  let loop (hash :: Word32) = do
        let !carsGotten = bruteForceHashCarIndicesOnly cars events hash

        sequence_ [BS.hPut stdout (BS.concat (BL.toChunks (encode (fromIntegral car :: Word16)))) | car <- carsGotten]
        hFlush stdout

        when (hash < maxBound) (loop (hash + 1))

  loop 0

  BS.hPut stdout (BS.pack [0xFF, 0xFF])
  hFlush stdout

runNormal :: IO ()
runNormal = do
  putStrLn "Gran Turismo 4 Spec II v1.09 Prize Car Randomizer Fairness Analyzer"

  (cars@(numCars, carArray), events, _) <- loadData
  allCarsGotten :: IOUArray Int Word64 <- newArray (0 :: Int, numCars - 1) (0 :: Word64)

  let loop (hash :: Word32) = do
        let !carsGotten = bruteForceHashCarIndicesOnly cars events hash

        sequence_ [modifyArray' allCarsGotten car (+ 1) | car <- carsGotten]
        when ((hash > 0) && (hash `mod` 100000 == 0)) (putStrLn (concat ["[!] Searched ", show hash, " hashes out of ", show (maxBound :: Word32), "."]))

        when (hash < maxBound) (loop (hash + 1))

  loop 0

  putStrLn "All Cars Obtained:\n\n"
  sequence_ [ readArray allCarsGotten ix >>= \ count ->
                TI.putStrLn (T.concat ["-", name car, ": ", T.pack (show count), "x"])
            | (ix, car) <- assocs carArray
            ]

main :: IO ()
main = do
  args <- getArgs
  let simple = foldr (\ x -> (((fmap toLower x == "-s") || (fmap toLower x == "--simple")) ||)) False args

  if simple then runSimple else runNormal