{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.IO
import S2RA.S2Data
import S2RA.Typedefs

-- Main brute-forcing flow, just to show all of the prize cars.
main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.07 Prize Car Randomizer Brute-Forcer\nMade by Azullia / 0xFC963F18DC21\nSpecial Thanks to Nenkai, TeaKanji\n"

  sp2Data <- loadData

  loop sp2Data
  where
    loop s2d@(_, _, necessities) = do
      putStr "Enter an in-game username (leave empty to exit): "
      hFlush stdout
      username <- getLine

      case username of
        "" -> return ()
        _  -> do
          TI.putStrLn ""
          results <- sequence $ bruteForce username s2d $ \ r c ->
            (r, c) <$ TI.putStrLn (T.concat [T.justifyRight 44 ' ' r, " ==> ", name c, " (", T.pack (show (viability c)),")"])
          TI.putStrLn ""
          TI.putStr (summarise username necessities results)
          TI.putStrLn ""
          loop s2d
