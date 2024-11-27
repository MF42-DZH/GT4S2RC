{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as L
import Data.Maybe ( fromMaybe )
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import S2RA.DataFiles
import Text.Read ( readMaybe )

main :: IO ()
main = do
  labels        <- T.lines <$> TI.readFile (dataFilePath Labels)
  cars          <- T.lines <$> TI.readFile (dataFilePath CarList)
  viabilities   <- (++ repeat 0) . fmap read . lines <$> readFile (dataFilePath Viabilities)
  necessaryFors <- (++ repeat []) . fmap (fromMaybe [] . readMaybe @[Int] . (++ "]") . ('[' :)) . lines <$> readFile (dataFilePath NecessityValues)
  necessities   <- TI.readFile (dataFilePath NecessityList)
  trackLabels   <- lines <$> readFile (dataFilePath TrackIds)
  trackNames    <- T.lines <$> TI.readFile (dataFilePath TrackNames)
  trackClasses  <- fmap (read @TrackClass) . lines <$> readFile (dataFilePath TrackClasses)
  raceLabels    <- lines <$> readFile (dataFilePath SingleRaceIDs)
  raceNames     <- T.lines <$> TI.readFile (dataFilePath SingleRaceNames)
  raceClasses   <- fmap (read @TrackClass) . lines <$> readFile (dataFilePath SingleRaceClasses)

  groups <- T.lines <$> TI.readFile (dataFilePath RaceList)
  funcs  <- T.lines <$> TI.readFile (dataFilePath FuncList)
  events <- (++ repeat "") . T.lines <$> TI.readFile (dataFilePath RaceNames)

  let allCars   = L.zipWith4 Car (fmap T.unpack labels) cars viabilities necessaryFors
      allEvents = zipWith3 Event (fmap T.unpack groups) (fmap T.unpack funcs) events
      allTracks = zipWith3 Track trackLabels trackNames trackClasses
      allRaces  = zipWith3 Race raceLabels raceNames raceClasses

  TI.writeFile combinedCarList (T.unlines (fmap (T.pack . show) allCars))
  TI.writeFile combinedEventList (T.unlines (fmap (T.pack . show) allEvents))
  TI.writeFile topLevelNecessities necessities
  TI.writeFile combinedTrackList (T.unlines (fmap (T.pack . show) allTracks))
  TI.writeFile combinedRaceData (T.unlines (fmap (T.pack . show) allRaces))
