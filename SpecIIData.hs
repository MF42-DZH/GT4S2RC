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
type EventInfo = [Event]
type SP2Data   = (CarInfo, EventInfo)
type BFData    = (CarInfo, [Combo])
type Combo     = (Event, String)
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

loadCombos :: String -> EventInfo -> [Combo]
loadCombos username es = zipWith (\ r f -> (r, username ++ group r ++ f)) es (fmap func es)

loadData :: IO SP2Data
loadData = do
  (cs, es) <- loadCarsAndEvents
  let cl = length cs
      el = length es
  return ((cl, listArray (0, cl) cs), es)

bruteForce' :: BFData -> [(String, Car)]
bruteForce' ((len, cars), combos) = fmap (\ (r, c) -> (coalesce event group r, cars ! randInt (fnv1a c) 0 len)) combos

coalesce :: (a -> String) -> (a -> String) -> a -> String
coalesce f g x =
  case f x of
    "" -> g x
    s  -> s

bruteForce :: String -> SP2Data -> (String -> Car -> a) -> [a]
bruteForce username (carInfo, eventInfo) action =
  let combos = loadCombos username eventInfo
  in  fmap (uncurry action) (bruteForce' (carInfo, combos))

summarise :: String -> [(String, Car)] -> String
summarise username is' = concat
  [ "--- \"", username , "\" :: Summary ---\n\n"
  , "Average Viability: ", show avgV, "\n"
  , "       Duplicates:\n"
  , unlines ( let counts   = fmap (\ (n, (count, _)) -> concat [n, " (x", show count, ")"]) (M.assocs duplicates)
                  allRaces = fmap (\ (n, (_, races)) -> L.intercalate ", " races) (M.assocs duplicates)
              in  zipWith (\ c rs -> concat [leftPad (maximum (fmap length counts) + 1) ' ' c, " :: ", rs]) counts allRaces
            )
  ]
  where
    infos = rlbd is'
    len   = length infos

    count p = foldr (\ x -> if p x then (+ 1) else id) (0 :: Int)

    rlbd []       = []
    rlbd (i@(r, _) : is)
      | "Duplicate" `L.isInfixOf` r = rlbd is
      | "Unused" `L.isInfixOf` r    = rlbd is
      | otherwise                   = i : rlbd is

    avgV       = fromIntegral (foldl' (\ acc (_, c) -> acc + viability c) 0 infos) / fromIntegral len
    duplicates =
      let combos = fmap (second name) infos
          counts = foldr (\ (r, n) acc -> M.alter (maybe (Just (1 :: Int, [r])) (Just . bimap (+ 1) (r :))) n acc) M.empty combos
      in  M.filter ((> 1) . fst) counts

leftPad :: Int -> Char -> String -> String
leftPad minLength padding input = replicate (max 0 (minLength - length input)) padding ++ input

-- Main brute-forcing flow, just to show all of the prize cars.
main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.06.X Prize Car Randomizer Brute-Forcer\n"

  sp2Data <- loadData
  loop sp2Data
  where
    loop s2d = do
      putStr "Enter an in-game username (leave empty to exit): "
      hFlush stdout
      username <- getLine

      case username of
        "" -> return ()
        _  -> do
          putStrLn ""
          results <- sequence $ bruteForce username s2d $ \ r c ->
            (r, c) <$ putStrLn (concat [leftPad 44 ' ' r, " ==> ", name c, " (", show (viability c),")"])
          putStrLn ""
          putStr (summarise username results)
          putStrLn ""
          loop s2d
