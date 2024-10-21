module S2RA.Bruteforce where

import Prelude hiding ( readIO )
import Control.Concurrent.STM
import Control.Monad
import Data.IORef
import qualified Data.Set as S
import Data.Word
import S2RA.Concurrent
import S2RA.Typedefs
import S2RA.S2Data

type Evaluation = (Username, Double, Int)
type Cmp p      = p -> p -> Bool

charset :: String
charset = ['A'..'Z'] ++ ['a'..'z'] ++ " 0123456789`'\\;,.[]/-=!@#$^&*()~|:<>?_+{}"

determineViability :: Bool -> Necessities -> Username -> S2Data -> Evaluation
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
formatWinner !tn (!u, !v, !m) = concat ["[", tn, "]: New best for criteria ", show v, " from username \"", u, "\", missing ", show m, " cars for 100%."]

isWinner :: Cmp Double -> IORef Evaluation -> Evaluation -> IO (Maybe Evaluation)
isWinner !p !mx c@(_, v, _) = do
  (_, v', _) <- readIORef mx
  return $ if p v v' then Just c else Nothing

determine :: Cmp Double -> IORef Evaluation -> Bool -> Necessities -> Username -> S2Data -> IO (Maybe Evaluation)
determine !p !mx !dbm !ns !un !sp2Data =
  let vd = determineViability dbm ns un sp2Data
  in  do
    w <- isWinner p mx vd
    forM_ w (writeIORef mx)
    return w

zipF :: Applicative f => f a -> f b -> f (a, b)
zipF a b = (,) <$> a <*> b

class IOModifiable var where
  readIO    :: var a -> IO a
  writeIO   :: var a -> a -> IO ()
  modifyIO  :: var a -> (a -> a) -> IO ()
  modifyIO' :: var a -> (a -> a) -> IO () -- Strict version of modifyIO

instance IOModifiable IORef where
  readIO    = readIORef
  writeIO   = writeIORef
  modifyIO  = modifyIORef
  modifyIO' = modifyIORef'

instance IOModifiable MVar where
  readIO         = readMVar
  writeIO        = (void .) . swapMVar
  modifyIO mv f  = modifyMVar_ mv (pure . f)
  modifyIO' mv f = modifyMVar_ mv (pure . f)

instance IOModifiable TVar where
  readIO    = readTVarIO
  writeIO   = (atomically .) . writeTVar
  modifyIO  = (atomically .) . modifyTVar
  modifyIO' = (atomically .) . modifyTVar'

data WorkerInfo n otherParams = WorkerInfo
  { wiThreadName  :: !String
  , wiCmp         :: Cmp n
  , wiBest        :: IORef (Username, n, Int)
  , wiData        :: !S2Data
  , wiNecessities :: !Necessities
  , wiOtherParams :: otherParams
  }

-- Please make sure to use joinHandle or joinHandle_ to join your workers.
baseWorker :: (WorkerInfo n op -> IO ()) -> WorkerInfo n op -> IO (JoinHandle ())
baseWorker action wi = forkJoinable (action wi)

class UsernameQueue us
instance UsernameQueue [String]
instance UsernameQueue (TBQueue String)

-- Do not construct directly. Use the camel-case smart constructor below.
data ViabilityParams cs uns = ViabilityParams
  { divideByMissing :: !Bool
  , shouldContinue  :: cs
  , usernameQueue   :: uns
  }

class ContinueSource t where
  continue :: t -> IO Bool

instance ContinueSource Bool where
  continue = pure

instance IOModifiable var => ContinueSource (var Bool) where
  continue = readIO

viabilityParams :: (UsernameQueue us, ContinueSource cs) => Bool -> cs -> us -> ViabilityParams cs us
viabilityParams = ViabilityParams

viabilityWorker :: WorkerInfo Double (ViabilityParams Bool [String]) -> IO (JoinHandle ())
viabilityWorkerSTM :: WorkerInfo Double (ViabilityParams (MVar Bool) (TBQueue String)) -> IO (JoinHandle ())

(viabilityWorker, viabilityWorkerSTM) = (baseWorker nonStm, baseWorker withStm)
  where
    nonStm (WorkerInfo name p mx s2Data ns (ViabilityParams !dbm _ (uns :: [String]))) = do
      haveSearchedRaw <- newIORef (0 :: Word64)
      haveSearchedMod <- newIORef (0 :: Word64)

      let printer u = do
            modifyIORef' haveSearchedRaw (+ 1)
            modifyIORef' haveSearchedMod ((`mod` 50000) . (+ 1))

            zipF (readIORef haveSearchedMod) (readIORef haveSearchedRaw) >>= \ (h, r) ->
              when (h == 0) $ do
                putStrLn (concat ["[", name, "]: Searched ", show r, " names. Currently looking at: \"", u, "\"."])
                threadDelay 1000000

      forM_ uns $ \ un -> do
        result <- determine p mx dbm ns un s2Data
        forM_ result $ \ vs -> putStrLn (formatWinner name vs)
        printer un
      
      summarizeSearch name haveSearchedRaw mx

    withStm (WorkerInfo name p mx s2Data ns (ViabilityParams !dbm shouldContinue (uns :: TBQueue String))) = do
      haveSearchedRaw <- newTVarIO (0 :: Word64)
      haveSearchedMod <- newTVarIO (0 :: Word64)

      let printer u = do
            atomically $ do
              modifyTVar' haveSearchedRaw (+ 1)
              modifyTVar' haveSearchedMod ((`mod` 50000) . (+ 1))

            atomically (zipF (readTVar haveSearchedMod) (readTVar haveSearchedRaw)) >>= \ (h, r) ->
              when (h == 0) $ do
                putStrLn (concat ["[", name, "]: Searched ", show r, " names. Currently looking at: \"", u, "\"."])
                threadDelay 1000000
          analysisLoop = do
            !mu <- atomically (tryReadTBQueue uns)

            forM_ mu $ \ !un -> do
              result <- determine p mx dbm ns un s2Data
              forM_ result $ \ !vs -> putStrLn (formatWinner name vs)
              printer un

            zipF (readMVar shouldContinue) (atomically (not <$> isEmptyTBQueue uns)) >>= (`when` analysisLoop) . uncurry (||)

      analysisLoop

      summarizeSearch name haveSearchedRaw mx

    summarizeSearch name raw mx = zipF (readIO raw) (readIO mx) >>= \ (r, (n, v, m)) -> putStrLn $ concat
      [ "[", name, "]: Searched a total of "
      , show r
      , " names.\n"
      , "[", name, "]: Best result "
      , show v
      , " from username \""
      , n
      , "\", missing "
      , show m
      , " cars for 100%."
      ]

split :: [a] -> ([a], [a])
split = go False
  where go _ []           = ([], [])
        go False (x : xs) = let (l, r) = go True xs in (x : l, r)
        go True (x : xs) = let (l, r)  = go False xs in (l, x : r)

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
