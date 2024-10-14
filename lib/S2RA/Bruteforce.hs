module S2RA.Bruteforce where

import Control.Monad
import Data.IORef
import qualified Data.Set as S
import S2RA.Concurrent
import S2RA.Typedefs
import S2RA.S2Data

type Evaluation = (Username, Double, Int)
type Cmp p      = p -> p -> Bool

charset :: String
charset = ['A'..'Z'] ++ ['a'..'z'] ++ " 0123456789`'\\;,.[]/-=!@#$^&*()~|:<>?_+{}"

determineViability :: Bool -> Necessities -> Username -> SP2Data -> Evaluation
determineViability dbm ns username sp2Data =
  let infoF   = bruteForce username sp2Data (const id)
      vs      = fmap viability infoF
      penalty = 1 + S.size (missingFor100 infoF ns)
  in  ( username
      , if dbm
        then fromIntegral (sum vs) / (fromIntegral (length vs) * fromIntegral (penalty * penalty))
        else fromIntegral (sum vs) / fromIntegral (length vs)
      , penalty - 1
      )

formatWinner :: String -> Evaluation -> String
formatWinner tn (u, v, m) = concat ["[", tn, "]: New best for criteria ", show v, " from username \"", u, "\", missing ", show m, " cars for 100%."]

isWinner :: Cmp Double -> IORef Evaluation -> Evaluation -> IO (Maybe Evaluation)
isWinner p mx c@(_, v, _) = do
  (_, v', _) <- readIORef mx
  return $ if p v v' then Just c else Nothing

determine :: Cmp Double -> IORef Evaluation -> Bool -> Necessities -> Username -> SP2Data -> IO (Maybe Evaluation)
determine p mx dbm ns un sp2Data =
  let vd = determineViability dbm ns un sp2Data
  in  do
    w <- isWinner p mx vd
    forM_ w (writeIORef mx)
    return w

worker :: String -> Cmp Double -> IORef Evaluation -> Bool -> Necessities -> [Username] -> SP2Data -> IO (JoinHandle ())
worker name p mx dbm ns uns sp2Data = forkJoinable $ forM_ uns $ \ un -> do
  result <- determine p mx dbm ns un sp2Data
  forM_ result $ \ vs -> putStrLn (formatWinner name vs)

generateDatasets :: Int -> ([String], [String], [String], [String], [String], [String])
generateDatasets n
  | n <= 1    = ( fmap pure ['A'..'M']
                , fmap pure ['N'..'Z']
                , fmap pure ['a'..'m']
                , fmap pure ['n'..'z']
                , fmap pure " 0123456789`'\\;,.[]/"
                , fmap pure "-=!@#$^&*()~|:<>?_+{}"
                )
  | otherwise = let (d1, d2, d3, d4, d5, d6) = generateDatasets (n - 1)
                in  ( d1 ++ ((:) <$> charset <*> d1)
                    , d2 ++ ((:) <$> charset <*> d2)
                    , d3 ++ ((:) <$> charset <*> d3)
                    , d4 ++ ((:) <$> charset <*> d4)
                    , d5 ++ ((:) <$> charset <*> d5)
                    , d6 ++ ((:) <$> charset <*> d6)
                    )
