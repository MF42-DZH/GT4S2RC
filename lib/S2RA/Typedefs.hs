module S2RA.Typedefs where

import Data.Array
import Data.Char
import qualified Data.List as L
import Data.Text ( Text )
import qualified Data.Text as T
import Text.ParserCombinators.ReadP

type Label       = String
type Name        = Text
type Viability   = Int
type Func        = String
type Necessity   = Text
type Necessities = Array Int Necessity

data Car = Car
  { label        :: Label
  , name         :: Name
  , viability    :: Viability
  , necessaryFor :: [Int] -- Indices into the necessity array found in loadNecessities.
  }
  deriving (Eq, Ord)

data Event = Event
  { group :: Label
  , func  :: Func
  , event :: Name
  }
  deriving (Eq, Ord)

data TrackClass
  = SixCars
  | RoadRally
  | DirtRally
  | SnowRally
  deriving (Eq, Ord, Enum, Bounded)

data Track = Track
  { tlabel :: Label
  , tname  :: Name
  , tclass :: TrackClass
  }
  deriving (Eq, Ord)

data Race = Race
  { rlabel :: Label
  , rname  :: Name
  , rclass :: TrackClass
  }
  deriving (Eq, Ord)

instance Show Car where
  show car = label car
    <> "|" <> T.unpack (name car)
    <> "|" <> show (viability car)
    <> "|" <> L.intercalate "," (fmap show (necessaryFor car))

carP :: ReadP Car
carP = do
  l  <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  n  <- T.pack <$> manyTill (satisfy (/= '|')) (satisfy (== '|'))
  v  <- read <$> manyTill (satisfy (/= '|')) (satisfy (== '|'))
  nf <- sepBy (read <$> many1 (satisfy isDigit)) (char ',') <* eof
  return (Car l n v nf)

instance Read Car where
  readsPrec _ = readP_to_S carP

instance Show Event where
  show e = group e
    <> "|" <> func e
    <> "|" <> T.unpack (event e)

eventP :: ReadP Event
eventP = do
  g <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  f <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  e <- T.pack <$> manyTill (satisfy (/= '|')) eof
  return (Event g f e)

instance Read Event where
  readsPrec _ = readP_to_S eventP

instance Show TrackClass where
  show SixCars   = "6"
  show RoadRally = "R"
  show DirtRally = "D"
  show SnowRally = "S"

trackClassP :: ReadP TrackClass
trackClassP = choice
  [ SixCars <$ char '6'
  , RoadRally <$ char 'R'
  , DirtRally <$ char 'D'
  , SnowRally <$ char 'S'
  ]

instance Read TrackClass where
  readsPrec _ = readP_to_S trackClassP

instance Show Track where
  show t = tlabel t
    <> "|" <> T.unpack (tname t)
    <> "|" <> show (tclass t)

trackP :: ReadP Track
trackP = do
  l <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  n <- T.pack <$> manyTill (satisfy (/= '|')) (satisfy (== '|'))
  c <- read <$> manyTill (satisfy (/= '|')) eof
  return (Track l n c)

instance Read Track where
  readsPrec _ = readP_to_S trackP

instance Show Race where
  show r = rlabel r
    <> "|" <> T.unpack (rname r)
    <> "|" <> show (rclass r)

raceP :: ReadP Race
raceP = do
  l <- manyTill (satisfy (/= '|')) (satisfy (== '|'))
  n <- T.pack <$> manyTill (satisfy (/= '|')) (satisfy (== '|'))
  c <- read <$> manyTill (satisfy (/= '|')) eof
  return (Race l n c)

instance Read Race where
  readsPrec _ = readP_to_S raceP
