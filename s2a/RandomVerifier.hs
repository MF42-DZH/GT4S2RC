module Main where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.Foldable
import qualified Data.Text as T
import Data.Word
import S2RA.Concurrent
import S2RA.CRCPRNG ( randInt )
import S2RA.S2Data
import S2RA.Typedefs

main :: IO ()
main = do
  ((_, cars), _, _) <- loadData

  let d1 = [0 :: Word32 .. (maxBound `div` 4) - 1]
      d2 = [(maxBound `div` 4) :: Word32 .. (maxBound `div` 2) - 1]
      d3 = [(maxBound `div` 2) :: Word32 .. 3 * (maxBound `div` 4) - 1]
      d4 = [3 * (maxBound `div` 4) :: Word32 .. maxBound]

  cars1 :: IOUArray Int Word32 <- newArray (0, 742) 0
  cars2 :: IOUArray Int Word32 <- newArray (0, 742) 0
  cars3 :: IOUArray Int Word32 <- newArray (0, 742) 0
  cars4 :: IOUArray Int Word32 <- newArray (0, 742) 0

  combined :: IOUArray Int Word32 <- newArray (0, 742) 0

  traverse
    (\ (dataset, storage) -> forkJoinable $
      forM_ dataset $ \ s ->do
        when (s `mod` 100000 == 0) (putStrLn $ "Reading: " ++ show s)
        let r = randInt (s * 16777619) 0 743
        readArray storage r >>= writeArray storage r . (+ 1)
    )
    [(d1, cars1), (d2, cars2), (d3, cars3), (d4, cars4)]
    >>= mapM_ joinHandle_

  traverse_
    (\ storage -> forM_ [0 :: Int .. 742] $ \ ix -> do
      fs <- readArray storage ix
      fc <- readArray combined ix
      writeArray combined ix (fs + fc)
    )
    [cars1, cars2, cars3, cars4]

  forM_ [0 :: Int .. 742] $ \ ix -> do
    let car = cars ! ix
    amt <- readArray combined ix

    putStrLn $ concat [T.unpack (name car) , ": ", show amt]
