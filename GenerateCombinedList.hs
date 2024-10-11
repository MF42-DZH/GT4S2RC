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

import Text.ParserCombinators.ReadP

type Label     = String
type Name      = String
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
  show car = label car <> "|" <> name car <> "|" <> show (viability car)

carP :: ReadP Car
carP = do
  l <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  n <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  v <- manyTill (satisfy (/= '|')) eof
  return (Car l n (read v))

instance Read Car where
  readsPrec _ = readP_to_S carP

instance Show Event where
  show e = group e <> "|" <> func e <> "|" <> event e

eventP :: ReadP Event
eventP = do
  g <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  f <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  e <- manyTill (satisfy (/= '|')) eof
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
  labels      <- lines <$> readFile "Data/LABELS.txt"
  cars        <- lines <$> readFile "Data/CARLIST.txt"
  viabilities <- (++ repeat 0) . fmap read . lines <$> readFile "Data/VIABILITY.txt"

  groups <- lines <$> readFile "Data/RACELIST.txt"
  funcs  <- lines <$> readFile "Data/FUNCLIST.txt"
  events <- (++ repeat "") . lines <$> readFile "Data/RACENAMES.txt"

  let allCars   = zipWith3 Car labels cars viabilities
      allEvents = zipWith3 Event groups funcs events

  writeFile combinedCarList (unlines (fmap show allCars))
  writeFile combinedEventList (unlines (fmap show allEvents))
