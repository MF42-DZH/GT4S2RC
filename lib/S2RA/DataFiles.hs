module S2RA.DataFiles
  ( loadCarsAndEvents
  , loadAllNecessities
  , dataFilePath
  , combinedCarList
  , combinedEventList
  , topLevelNecessities
  , combinedTrackList
  , combinedRaceData
  , DataFile(..)
  , module S2RA.Typedefs
  ) where

import Data.Array
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import S2RA.Typedefs

combinedCarList, combinedEventList, topLevelNecessities, combinedTrackList, combinedRaceData :: String
combinedCarList     = "COMBINEDCARLIST.txt"
combinedEventList   = "COMBINEDEVENTLIST.txt"
topLevelNecessities = "NECESSITIES.txt"
combinedTrackList   = "COMBINEDTRACKLIST.txt"
combinedRaceData    = "COMBINEDRACEDATA.txt"

data DataFile
  = Labels
  | CarList
  | Viabilities
  | NecessityList
  | NecessityValues
  | RaceList
  | FuncList
  | RaceNames
  | TrackIds
  | TrackNames
  | TrackClasses
  | SingleRaceIDs
  | SingleRaceNames
  | SingleRaceClasses
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

dataFilePath :: DataFile -> String
dataFilePath Labels            = "Data/LABELS.txt"
dataFilePath CarList           = "Data/CARLIST.txt"
dataFilePath Viabilities       = "Data/VIABILITY.txt"
dataFilePath NecessityList     = "Data/NECESSITY.txt"
dataFilePath NecessityValues   = "Data/NECESSITYVALUES.txt"
dataFilePath RaceList          = "Data/RACELIST.txt"
dataFilePath FuncList          = "Data/FUNCLIST.txt"
dataFilePath RaceNames         = "Data/RACENAMES.txt"
dataFilePath TrackIds          = "Data/TRACKIDS.txt"
dataFilePath TrackNames        = "Data/TRACKNAMES.txt"
dataFilePath TrackClasses      = "Data/TRACKCLASS.txt"
dataFilePath SingleRaceIDs     = "Data/SINGLERACEIDS.txt"
dataFilePath SingleRaceNames   = "Data/SINGLERACENAMES.txt"
dataFilePath SingleRaceClasses = "Data/SINGLERACECLASS.txt"

loadCarsAndEvents :: IO ([Car], [Event])
loadCarsAndEvents = (,)
  <$> (fmap read . lines <$> readFile combinedCarList)
  <*> (fmap read . lines <$> readFile combinedEventList)

loadAllNecessities :: IO Necessities
loadAllNecessities = do
  ns     <- T.lines <$> TI.readFile topLevelNecessities
  return (listArray (1, length ns) ns)

data TrackData = TrackData
  { sixCarInfo    :: (Int, Array Int Track)
  , roadRallyInfo :: (Int, Array Int Track)
  , dirtRallyInfo :: (Int, Array Int Track)
  , snowRallyInfo :: (Int, Array Int Track)
  }

loadTracksAndRaces :: IO (TrackData, [Race])
loadTracksAndRaces = do
  allTracks :: [Track] <- fmap read . lines <$> readFile combinedTrackList
  allRaces :: [Race]   <- fmap read . lines <$> readFile combinedRaceData
  
  return
    ( TrackData
      (toInfo (filter ((== SixCars) . tclass) allTracks))
      (toInfo (filter ((== RoadRally) . tclass) allTracks))
      (toInfo (filter ((== DirtRally) . tclass) allTracks))
      (toInfo (filter ((== SnowRally) . tclass) allTracks))
    , allRaces
    )
  where
    toInfo xs = (length xs, listArray (0, length xs - 1) xs)
