{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module S2RA.S2Data where

import Data.Array
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Data.String
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Word
import S2RA.CRCPRNG ( randInt )
import S2RA.DataFiles

type CarInfo   = (Int, Array Int Car)
type EventInfo = [Event]
type S2Data   = (CarInfo, EventInfo, Necessities)
type BFData    = (CarInfo, [Combo])
type Combo     = (Event, String)
type PrizeInfo = (Text, Car)
type Username  = String

fnv1a :: String -> Word32
fnv1a = foldl' (\ acc ch -> (acc `xor` fromIntegral (ord ch)) * prime) initial
  where
    initial = 0x811c9dc5
    prime   = 16777619

loadCombos :: Username -> EventInfo -> [Combo]
loadCombos username es = zipWith (\ r f -> (r, username <> group r <> f)) es (fmap func es)

loadData :: IO S2Data
loadData = do
  (!cs, !es) <- loadCarsAndEvents
  !ns        <- loadAllNecessities
  let !cl = length cs
  return ((cl, listArray (0, cl) cs), es, ns)

bruteForce' :: BFData -> [PrizeInfo]
bruteForce' ((len, cars), combos) = removeDuplicatesAndUnused $ fmap (\ (r, c) -> (coalesce event (T.pack . group) r, cars ! randInt (fnv1a c) 0 len)) combos

coalesce :: (Eq s, IsString s) => (a -> s) -> (a -> s) -> a -> s
coalesce f g x =
  case f x of
    "" -> g x
    s  -> s

-- Expensive method call.
removeDuplicatesAndUnused :: [PrizeInfo] -> [PrizeInfo]
removeDuplicatesAndUnused []       = []
removeDuplicatesAndUnused (i@(r, _) : is)
  | "Duplicate" `T.isInfixOf` r  = removeDuplicatesAndUnused is
  | "Unused" `T.isInfixOf` r     = removeDuplicatesAndUnused is
  | "Inaccurate" `T.isInfixOf` r = removeDuplicatesAndUnused is
  | otherwise                    = i : removeDuplicatesAndUnused is

bruteForce :: Username -> S2Data -> (Text -> Car -> a) -> [a]
bruteForce username (carInfo, eventInfo, _) action =
  let combos = loadCombos username eventInfo
  in  fmap (uncurry action) (bruteForce' (carInfo, combos))

missingFor100 :: [Car] -> Necessities -> Set Necessity
missingFor100 cars necessities =
  let sn = S.fromList (elems necessities)
  in  foldl' (\ s c -> foldl' (flip S.delete) s (fmap (necessities !) (necessaryFor c))) sn cars

duplicateEvents :: [PrizeInfo] -> Map Car [Text]
duplicateEvents infos =
  let ds = foldl' (\ m (t, c) -> M.alter (maybe (Just [t]) (Just . (t :))) c m ) M.empty infos
  in  M.filter (\ case
        (_ : _ : _) -> True
        _           -> False) ds

duplicateCounts :: [Car] -> Map Car Int
duplicateCounts cars =
  let dcs = foldl' (flip (M.alter (maybe (Just 1) (Just . (+ 1))))) M.empty cars
  in  M.filter (> 1) dcs

duplicateDetails :: [PrizeInfo] -> Map Name (Int, [Text])
duplicateDetails infos =
  let combos = fmap (second name) infos
      counts = foldr (\ (r, n) acc -> M.alter (maybe (Just (1 :: Int, [r])) (Just . bimap (+ 1) (r :))) n acc) M.empty combos
  in  M.filter ((> 1) . fst) counts

summarise :: Username -> Necessities -> [PrizeInfo] -> Text
summarise username ns is' = T.concat
  [ "--- \"", T.pack username , "\" :: Summary ---\n\n"
  , "Average Viability: ", T.pack (show avgV), "\n"
  , "\nDuplicates:\n"
  , T.unlines ( let counts   = fmap (\ (n, (count, _)) -> T.concat [n, " (x", T.pack (show count), ")"]) (M.assocs dups)
                    allRaces = fmap (\ (_, (_, races)) -> T.intercalate ", " races) (M.assocs dups)
                in  zipWith (\ c rs -> T.concat [T.justifyRight (maximum (fmap T.length counts) + 1) ' ' c, " :: ", rs]) counts allRaces
              )
  , "\nMissing for 100%:\n"
  ,  case S.elems (missingFor100 (fmap snd is') ns) of
       []      -> "*** None. This is a 100% randomizer playthrough candidate! ***"
       missing -> T.unlines (fmap ("- " <>) missing)
  ]
  where
    infos = is'
    len   = length infos

    avgV :: Double = fromIntegral (foldl' (\ acc (_, c) -> acc + viability c) 0 infos) / fromIntegral len
    dups           = duplicateDetails infos
