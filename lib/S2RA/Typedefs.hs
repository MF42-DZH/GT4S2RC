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
