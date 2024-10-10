module SpecIIData where

import Control.Monad
import CRCPRNG ( randInt )
import Data.Array
import Data.Bifunctor
import Data.Bits
import Data.Char
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
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
  , "       Duplicates:\n"
  , unlines ( let counts   = fmap (\ (n, (count, _)) -> concat [n, " (x", show count, ")"]) (M.assocs duplicates)
                  allRaces = fmap (\ (n, (_, races)) -> L.intercalate ", " races) (M.assocs duplicates)
              in  zipWith (\ c rs -> concat [leftPad (maximum (fmap length counts) + 1) ' ' c, " :: ", rs]) counts allRaces
            )
  ]
  where
    len = length infos

    count p = foldr (\ x -> if p x then (+ 1) else id) (0 :: Int)

    rlbd []       = []
    rlbd (i@(r@('l' : _), _) : is)
      | count (== r) (fmap fst is) == 3 = rlbd is
      | otherwise                       = i : rlbd is
    rlbd (i : is) = i : rlbd is

    avgV       = fromIntegral (foldl' (\ acc (_, c) -> acc + viability c) 0 infos) / fromIntegral len
    duplicates =
      let combos = fmap (second name) (rlbd infos)
          counts = foldr (\ (r, n) acc -> M.alter (maybe (Just (1 :: Int, [r])) (Just . bimap (+ 1) (r :))) n acc) M.empty combos
      in  M.filter ((> 1) . fst) counts

leftPad :: Int -> Char -> String -> String
leftPad minLength padding input = replicate (max 0 (minLength - length input)) padding ++ input

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
            (r, c) <$ putStrLn (concat [leftPad 30 ' ' r, " ==> ", name c, " (", show (viability c),")"])
          putStrLn ""
          putStr (summarise username results)
          putStrLn ""
          loop cs rs fs
