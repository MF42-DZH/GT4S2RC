module S2RA.DataFiles
  ( loadCarsAndEvents
  , loadAllNecessities
  , dataFilePath
  , combinedCarList
  , combinedEventList
  , topLevelNecessities
  , DataFile(..)
  , module S2RA.Typedefs
  ) where

import Data.Array
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import S2RA.Typedefs

combinedCarList, combinedEventList, topLevelNecessities :: String
combinedCarList     = "COMBINEDCARLIST.txt"
combinedEventList   = "COMBINEDEVENTLIST.txt"
topLevelNecessities = "NECESSITIES.txt"

data DataFile
  = Labels
  | CarList
  | Viabilities
  | NecessityList
  | NecessityValues
  | RaceList
  | FuncList
  | RaceNames
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

dataFilePath :: DataFile -> String
dataFilePath Labels          = "Data/LABELS.txt"
dataFilePath CarList         = "Data/CARLIST.txt"
dataFilePath Viabilities     = "Data/VIABILITY.txt"
dataFilePath NecessityList   = "Data/NECESSITY.txt"
dataFilePath NecessityValues = "Data/NECESSITYVALUES.txt"
dataFilePath RaceList        = "Data/RACELIST.txt"
dataFilePath FuncList        = "Data/FUNCLIST.txt"
dataFilePath RaceNames       = "Data/RACENAMES.txt"

loadCarsAndEvents :: IO ([Car], [Event])
loadCarsAndEvents = (,)
  <$> (fmap read . lines <$> readFile combinedCarList)
  <*> (fmap read . lines <$> readFile combinedEventList)

loadAllNecessities :: IO Necessities
loadAllNecessities = do
  ns     <- T.lines <$> TI.readFile topLevelNecessities
  return (listArray (1, length ns) ns)
