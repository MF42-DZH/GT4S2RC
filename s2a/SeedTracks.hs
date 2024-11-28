{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TI
import System.IO
import S2RA.DataFiles
import S2RA.S2Data

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.08 Track Randomizer Brute-Forcer\nMade by Azullia / 0xFC963F18DC21\nSpecial Thanks to Nenkai, TeaKanji\n"
  tracksAndRaces <- loadTracksAndRaces

  loop tracksAndRaces
  where
    loop tar@(ts, rs) = do
      putStr "Enter an in-game username (leave empty to exit): "
      hFlush stdout
      username <- getLine

      case username of
        "" -> return ()
        _  -> do
          TI.putStrLn ""
          sequence_ $ bruteForceTracks username ts rs $ \ r t ->
            TI.putStrLn (T.concat [T.justifyRight 47 ' ' (rname r), " ==> ", tname t])
          TI.putStrLn ""
          loop tar