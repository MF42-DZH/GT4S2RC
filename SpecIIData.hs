module SpecIIData where

import Control.Monad
import CRCPRNG ( randInt )
import Data.Array
import Data.Bits
import Data.Char
import Data.Word
import GenerateCombinedList
import System.IO

type CarInfo   = (Int, Array Int Car)
type Combo     = (String, String)
type PrizeInfo = (String, Car)

fnv1a :: String -> Word32
fnv1a str = go str 0x811c9dc5
  where
    prime = 16777619

    go [] acc     = acc
    go (c : cs) r =
      let r'  = r `xor` fromIntegral (ord c)
          r'' = r' * prime
      in  go cs r''

loadCombos :: String -> [String] -> [String] -> [Combo]
loadCombos username = zipWith (\ r f -> (r, username ++ r ++ f))

loadCars :: IO CarInfo
loadCars = do
  ls <- fmap read . lines <$> readFile "COMBINEDLIST.txt"
  let len = length ls
  return (len, listArray (0, len) ls)

bruteForce' :: CarInfo -> [Combo] -> [(String, Car)]
bruteForce' (len, cars) = fmap (\ (r, c) -> (r, cars ! randInt (fnv1a c) 0 len))

bruteForce :: String -> CarInfo -> [String] -> [String] -> (String -> Car -> a) -> [a]
bruteForce username carInfo races funcs action =
  let combos = loadCombos username races funcs
  in  fmap (uncurry action) (bruteForce' carInfo combos)

-- Main brute-forcing flow, just to show all of the prize cars.
main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Brute-Forcer\n"

  cars  <- loadCars
  races <- lines <$> readFile "RACELIST.txt"
  funcs <- lines <$> readFile "FUNCLIST.txt"

  loop cars races funcs
  where
    loop cs rs fs = do
      putStr "Enter an in-game username (leave empty to exit): "
      hFlush stdout
      username <- getLine

      case username of
        "" -> return ()
        _  -> do
          putStrLn ""
          sequence_ $ bruteForce username cs rs fs $ \ r c ->
            putStrLn (concat [r, " ==> ", name c, " (", show (viability c),")"])
          putStrLn ""
          loop cs rs fs
