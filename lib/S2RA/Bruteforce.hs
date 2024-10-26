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

type Evaluation n = (Username, n, Int)
type Cmp p        = p -> p -> Bool

charset :: String
charset = ['A'..'Z'] ++ ['a'..'z'] ++ " 0123456789`'\\;,.[]/-=!@#$^&*()~|:<>?_+{}"

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
  , wiBest        :: IORef (Evaluation n)
  , wiData        :: !S2Data
  , wiNecessities :: !Necessities
  , wiOtherParams :: otherParams
  }

formatWinner :: Show n => String -> Evaluation n -> String
formatWinner !tn (!u, !v, !m) = concat ["[", tn, "]: New best for criteria ", show v, " from username \"", u, "\", missing ", show m, " cars for 100%."]

isWinner :: Cmp n -> IORef (Evaluation n) -> Evaluation n -> IO (Maybe (Evaluation n))
isWinner !p !mx c@(_, v, _) = do
  (_, v', _) <- readIORef mx
  return $ if p v v' then Just c else Nothing

-- Typically, you'd write eval with the declaration of the worker info in scope so you can capture the
-- type information of the extra parameters to use in the eval function.
determine :: WorkerInfo n p -> ([PrizeInfo] -> Evaluation n) -> Username -> IO (Maybe (Evaluation n))
determine wi eval username =
  let prizes     = bruteForce @PrizeInfo username (wiData wi) (,)
      evaluation = eval prizes
  in  do
    w <- isWinner (wiCmp wi) (wiBest wi) evaluation
    forM_ w (writeIORef (wiBest wi))
    return w

zipF :: Applicative f => f a -> f b -> f (a, b)
zipF a b = (,) <$> a <*> b

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

determineV :: (ContinueSource cs, UsernameQueue us) => WorkerInfo Double (ViabilityParams cs us) -> Username -> IO (Maybe (Evaluation Double))
determineV wi username = determine wi (\ prizes ->
  let infoF                   = fmap snd prizes
      vs                      = fmap viability infoF
      penalty                 = 1 + S.size (missingFor100 infoF (wiNecessities wi))
      ViabilityParams dbm _ _ = wiOtherParams wi
  in ( username
     , if   dbm
       then fromIntegral (sum vs) / (fromIntegral (length vs) * fromIntegral (penalty * penalty))
       else fromIntegral (sum vs) / fromIntegral (length vs)
     , penalty - 1
     )
  ) username

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
    nonStm wi@(WorkerInfo name _ mx _ _ (ViabilityParams !_ _ (uns :: [String]))) = do
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
        result <- determineV wi un
        forM_ result $ \ vs -> putStrLn (formatWinner name vs)
        printer un
      
      summarizeSearch name haveSearchedRaw mx

    withStm wi@(WorkerInfo name _ mx _ _ (ViabilityParams !_ shouldContinue (uns :: TBQueue String))) = do
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
              result <- determineV wi un
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
