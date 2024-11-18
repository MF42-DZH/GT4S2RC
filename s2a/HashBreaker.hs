module Main where

import Control.Monad
import Data.Bits
import Data.Char
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Word
import System.IO
import S2RA.Bruteforce
import S2RA.Concurrent
import Text.Read ( readMaybe )

type Intermediate = (String, Int, Word32)

fnv1aModInverse :: Word32
-- Derivation: fromIntegral (modInverse (16777619 :: Integer) ((2 :: Integer) ^ (32 :: Integer)))
fnv1aModInverse = 899433627

-- We essentially want to get back to 0x811c9dc5.
attemptReversalWorker :: TQueue Intermediate -> MVar String -> Int -> IO (JoinHandle ())
attemptReversalWorker queue result maxLength = forkJoinable comp
  where
    analysis (soFar, len, currentHash)
      | len < maxLength =
        let potentials = fmap (\ c -> (c : soFar, len + 1, (currentHash * fnv1aModInverse) .^. fromIntegral (ord c))) charset
        in  case L.find (\ (_, _, h) -> h == 0x811c9dc5) potentials of
          Nothing     -> False <$ unless (len == maxLength - 1) (atomically (F.traverse_ (writeTQueue queue) potentials))
          Just (n, _, _) -> True <$ putMVar result n
      | otherwise = pure False

    comp = do
      mp    <- atomically (tryReadTQueue queue)
      case mp of
        Nothing    -> ((&&) <$> atomically (not <$> isEmptyTQueue queue) <*> isEmptyMVar result) >>= (`when` comp)
        Just inter -> ((||) <$> (not <$> isEmptyMVar result) <*> analysis inter) >>= (`unless` comp)

main :: IO ()
main = do
  putStrLn "Gran Turismo 4 Spec II v1.08 Prize Car Randomizer FNV-1a Brute-Forcer"

  putStr (concat ["Enter a username hash (0 to ", show (maxBound :: Word32), "): "])
  hFlush stdout
  hash <- getLine

  putStr "Enter a username length upper bound (1 to 30): "
  hFlush stdout
  maxLength <- read <$> getLine

  case hash of
    "" -> return ()
    _  -> case readMaybe hash :: Maybe Integer of
      Nothing -> putStrLn "\nPlease write a valid numeric hash value."
      Just x  | x < 0                                 -> putStrLn "\nPlease write a valid numeric hash value."
              | x > fromIntegral (maxBound :: Word32) -> putStrLn "\nPlease write a valid numeric hash value." 
              | otherwise                             -> do
                queue  <- newTQueueIO @Intermediate
                result <- newEmptyMVar @String

                atomically (writeTQueue queue ("", 0, fromIntegral x))
                replicateM 5 (attemptReversalWorker queue result maxLength) >>= F.traverse_ joinHandle_

                maybeName <- tryTakeMVar result
                case maybeName of
                  Nothing   -> putStrLn "\nNo valid username found..."
                  Just name -> putStrLn (concat ["\nUsername: \"", name, "\""])
