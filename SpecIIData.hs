module SpecIIData where

import Control.Monad ( forM_ )
import CRCPRNG ( randInt )
import Data.Array
import Data.Bits
import Data.Char
import Data.Word

fnv1a :: String -> Word32
fnv1a str = go str 0x811c9dc5
  where
    prime = 16777619

    go [] acc     = acc
    go (c : cs) r =
      let r'  = r `xor` fromIntegral (ord c)
          r'' = r' * prime
      in  go cs r''

loadCombos :: String -> IO [(String, String)]
loadCombos username = do
  races <- lines <$> readFile "RACELIST.txt"
  funcs <- lines <$> readFile "FUNCLIST.txt"
  return $ zipWith (\ r f -> (r, username ++ r ++ f)) races funcs

loadCars :: IO (Int, Array Int String)
loadCars = do
  ls <- lines <$> readFile "CARLIST.txt"
  let len = length ls
  return (len, listArray (0, len) ls)

bruteForce :: String -> IO ()
bruteForce username = do
  (len, cars) <- loadCars
  combos      <- loadCombos username

  forM_ combos $ \ (r, c) -> do
    let seed  = fnv1a c
        prize = cars ! fromIntegral (randInt seed 0 len)
    putStrLn (r ++ " ==> " ++ c ++ " ==> " ++ prize)