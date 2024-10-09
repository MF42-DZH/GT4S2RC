module GenerateCombinedList
  ( Label
  , Name
  , Viability
  , Car(..)
  ) where

import Text.ParserCombinators.ReadP

type Label     = String
type Name      = String
type Viability = Int

data Car = Car
  { label     :: String
  , name      :: Name
  , viability :: Viability
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

main :: IO ()
main = do
  labels      <- lines <$> readFile "LABELS.txt"
  cars        <- lines <$> readFile "CARLIST.txt"
  viabilities <- fmap read . lines <$> readFile "VIABILITY.txt"

  let allCars = zipWith3 Car labels cars viabilities

  writeFile "COMBINEDLIST.txt" (unlines (fmap show allCars))
