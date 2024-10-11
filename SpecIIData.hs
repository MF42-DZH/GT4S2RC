{-# LANGUAGE OverloadedStrings #-}

module SpecIIData where

import Control.Monad
import CRCPRNG ( randInt )
import Data.Array
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Word
import GenerateCombinedList
import System.IO

type CarInfo   = (Int, Array Int Car)
type EventInfo = [Event]
type SP2Data   = (CarInfo, EventInfo)
type BFData    = (CarInfo, [Combo])
type Combo     = (Event, Text)
type PrizeInfo = (Text, Car)
type Username  = Text

fnv1a :: Text -> Word32
fnv1a = T.foldl' (\ acc ch -> (acc `xor` fromIntegral (ord ch)) * prime) initial
  where
    initial = 0x811c9dc5
    prime   = 16777619

loadCombos :: Username -> EventInfo -> [Combo]
loadCombos username es = zipWith (\ r f -> (r, username <> group r <> f)) es (fmap func es)

loadData :: IO SP2Data
loadData = do
  (cs, es) <- loadCarsAndEvents
  let cl = length cs
      el = length es
  return ((cl, listArray (0, cl) cs), es)

bruteForce' :: BFData -> [(Text, Car)]
bruteForce' ((len, cars), combos) = removeDuplicatesAndUnused $ fmap (\ (r, c) -> (coalesce event group r, cars ! randInt (fnv1a c) 0 len)) combos

coalesce :: (a -> Text) -> (a -> Text) -> a -> Text
coalesce f g x =
  case f x of
    "" -> g x
    s  -> s

-- Expensive method call.
removeDuplicatesAndUnused :: [(Text, b)] -> [(Text, b)]
removeDuplicatesAndUnused []       = []
removeDuplicatesAndUnused (i@(r, _) : is)
  | "Duplicate" `T.isInfixOf` r = removeDuplicatesAndUnused is
  | "Unused" `T.isInfixOf` r    = removeDuplicatesAndUnused is
  | otherwise                   = i : removeDuplicatesAndUnused is

bruteForce :: Username -> SP2Data -> (Text -> Car -> a) -> [a]
bruteForce username (carInfo, eventInfo) action =
  let combos = loadCombos username eventInfo
  in  fmap (uncurry action) (bruteForce' (carInfo, combos))

summarise :: Username -> [(Text, Car)] -> Text
summarise username is' = T.concat
  [ "--- \"", username , "\" :: Summary ---\n\n"
  , "Average Viability: ", T.pack (show avgV), "\n"
  , "       Duplicates:\n"
  , T.unlines ( let counts   = fmap (\ (n, (count, _)) -> T.concat [n, " (x", T.pack (show count), ")"]) (M.assocs duplicates)
                    allRaces = fmap (\ (n, (_, races)) -> T.intercalate ", " races) (M.assocs duplicates)
                in  zipWith (\ c rs -> T.concat [leftPad (maximum (fmap T.length counts) + 1) ' ' c, " :: ", rs]) counts allRaces
              )
  ]
  where
    infos = is'
    len   = length infos

    avgV       = fromIntegral (foldl' (\ acc (_, c) -> acc + viability c) 0 infos) / fromIntegral len
    duplicates =
      let combos = fmap (second name) infos
          counts = foldr (\ (r, n) acc -> M.alter (maybe (Just (1 :: Int, [r])) (Just . bimap (+ 1) (r :))) n acc) M.empty combos
      in  M.filter ((> 1) . fst) counts

leftPad :: Int -> Char -> Text -> Text
leftPad minLength padding input = T.replicate (max 0 (minLength - T.length input)) (T.pack [padding]) <> input

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
      username <- TI.getLine

      case username of
        "" -> return ()
        _  -> do
          TI.putStrLn ""
          results <- sequence $ bruteForce username s2d $ \ r c ->
            (r, c) <$ TI.putStrLn (T.concat [leftPad 44 ' ' r, " ==> ", name c, " (", T.pack (show (viability c)),")"])
          TI.putStrLn ""
          TI.putStr (summarise username results)
          TI.putStrLn ""
          loop s2d
