{-# LANGUAGE OverloadedStrings #-}

module GenerateCombinedList
  ( Label
  , Name
  , Viability
  , Func
  , Car(..)
  , Event(..)
  , combinedCarList
  , combinedEventList
  , loadCarsAndEvents
  ) where

import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Text.ParserCombinators.ReadP

type Label     = String
type Name      = Text
type Viability = Int
type Func      = String

data Car = Car
  { label     :: Label
  , name      :: Name
  , viability :: Viability
  }

data Event = Event
  { group :: Label
  , func  :: Func
  , event :: Name
  }

instance Show Car where
  show car = label car <> "|" <> T.unpack (name car) <> "|" <> show (viability car)

carP :: ReadP Car
carP = do
  l <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  n <- T.pack <$> manyTill (satisfy (/= '|')) (satisfy (== '|'))
  v <- manyTill (satisfy (/= '|')) eof
  return (Car l n (read v))

instance Read Car where
  readsPrec _ = readP_to_S carP

instance Show Event where
  show e = group e <> "|" <> func e <> "|" <> T.unpack (event e)

eventP :: ReadP Event
eventP = do
  g <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  f <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  e <- T.pack <$> manyTill (satisfy (/= '|')) eof
  return (Event g f e)

instance Read Event where
  readsPrec _ = readP_to_S eventP

combinedCarList, combinedEventList :: String
combinedCarList   = "COMBINEDCARLIST.txt"
combinedEventList = "COMBINEDEVENTLIST.txt"

loadCarsAndEvents :: IO ([Car], [Event])
loadCarsAndEvents = (,)
  <$> (fmap read . lines <$> readFile combinedCarList)
  <*> (fmap read . lines <$> readFile combinedEventList)

main :: IO ()
main = do
  labels      <- T.lines <$> TI.readFile "Data/LABELS.txt"
  cars        <- T.lines <$> TI.readFile "Data/CARLIST.txt"
  viabilities <- (++ repeat 0) . fmap read . lines <$> readFile "Data/VIABILITY.txt"

  groups <- T.lines <$> TI.readFile "Data/RACELIST.txt"
  funcs  <- T.lines <$> TI.readFile "Data/FUNCLIST.txt"
  events <- (++ repeat "") . T.lines <$> TI.readFile "Data/RACENAMES.txt"

  let allCars   = zipWith3 Car (fmap T.unpack labels) cars viabilities
      allEvents = zipWith3 Event (fmap T.unpack groups) (fmap T.unpack funcs) events

  TI.writeFile combinedCarList (T.unlines (fmap (T.pack . show) allCars))
  TI.writeFile combinedEventList (T.unlines (fmap (T.pack . show) allEvents))
