{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Word
import System.Environment
import System.IO
import Text.Read ( readMaybe )
import S2RA.S2Data
import S2RA.Typedefs

-- Main brute-forcing flow, just to show all of the prize cars.
main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.08 Prize Car Randomizer Brute-Forcer\nMade by Azullia / 0xFC963F18DC21\nSpecial Thanks to Nenkai, TeaKanji\n"

  args <- getArgs
  let hashes = foldr (\ x -> (((fmap toLower x == "-h") || (fmap toLower x == "--hash")) ||)) False args

  when hashes $ putStrLn "Bruteforcing username hashes instead.\n"

  sp2Data <- loadData

  loop sp2Data hashes
  where
    loop s2d@(_, _, necessities) hashes
      | not hashes = do
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
            loop s2d hashes
      | otherwise = do
        putStr (concat ["Enter a username hash (0 to ", show (maxBound :: Word32), "; leave empty to exit): "])
        hFlush stdout
        hash <- getLine

        case hash of
          "" -> return ()
          _  -> case readMaybe hash :: Maybe Integer of
            Nothing -> putStrLn "Please write a valid numeric hash value.\n" >> loop s2d hashes
            Just x  | x < 0                                 -> putStrLn "Please write a valid numeric hash value.\n" >> loop s2d hashes
                    | x > fromIntegral (maxBound :: Word32) -> putStrLn "Please write a valid numeric hash value.\n" >> loop s2d hashes
                    | otherwise                             -> do
                      TI.putStrLn ""
                      results <- sequence $ bruteForceHash (fromIntegral x) s2d $ \ r c ->
                        (r, c) <$ TI.putStrLn (T.concat [T.justifyRight 44 ' ' r, " ==> ", name c, " (", T.pack (show (viability c)),")"])
                      TI.putStrLn ""
                      TI.putStr (summarise (show x) necessities results)
                      TI.putStrLn ""
                      loop s2d hashes
