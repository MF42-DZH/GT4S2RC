module SpecIIData where

import Control.Monad
import CRCPRNG ( randInt )
import Data.Array
import Data.Bits
import Data.Char
import qualified Data.List as L
import Data.Set ( Set )
import qualified Data.Set as S
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

summarise :: String -> [(String, Car)] -> String
summarise username infos = concat
  [ "--- \"", username , "\" :: Summary ---\n\n"
  , "Average Viability: ", show avgV, "\n"
  , "v Duplicates v\n"
  , unlines (fmap ("- " ++) duplicates)
  ]
  where
    len = length infos

    rlbd []       = []
    rlbd (i@(r@('l' : _), _) : is)
      | r `elem` fmap fst is = rlbd is
      | otherwise            = i : rlbd is
    rlbd (i : is) = i : rlbd is

    avgV       = fromIntegral (foldl' (\ acc (_, c) -> acc + viability c) 0 infos) / fromIntegral len
    duplicates =
      let names = fmap (name . snd) (rlbd infos)
      in  S.toList (S.fromList (names L.\\ S.toList (S.fromList names)))

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
          results <- sequence $ bruteForce username cs rs fs $ \ r c ->
            (r, c) <$ putStrLn (concat [r, " ==> ", name c, " (", show (viability c),")"])
          putStrLn ""
          putStrLn (summarise username results)
          putStrLn ""
          loop cs rs fs
