module S2RA.MT19937 where

-- NOT USED.
-- PD ONLY USED THIS IN GT5 ONWARDS.

import Data.Array
import Data.Bits
import Data.Word

data MTState = MTState
  { s_array :: Array Int Word32
  , s_index :: Int
  }

cF :: Word32
cF = 1812433253

cR :: Int
cR = 31

umask :: Word32
umask = 0x80000000

lmask :: Word32
lmask =  0x7fffffff

cW :: Word32
cW = 32

cN, cM, cU, cS, cT, cL :: Int
cN = 624
cM = 397
cU = 11
cS = 7
cT = 15
cL = 18

cA, cB, cC :: Word32
cA = 0x9908b0df
cB = 0x9d2c5680
cC = 0xefc60000

mkMTState :: Word32 -> MTState
mkMTState seed = MTState (populate 0 seed (array (0, cN - 1) [(i, 0) | i <- [0 .. cN - 1]])) 0
  where
    populate 0 s   = (// [(0, s)]) . populate 1 s
    populate 624 s = id
    populate n s   =
      let s' = cF * (s .^. (s `shiftR` (fromIntegral cW - 2))) + fromIntegral n
      in  (// [(n, s')]) . populate (n + 1) s'

mtNext :: MTState -> (Word32, MTState)
mtNext (MTState arr k) =
  let j    = case k - (cN - 1) of
        x | x < 0 -> x + cN
        x         -> x
      x    = ((arr ! k) .&. umask) .|. ((arr ! j) .&. lmask)
      xA   = case x .&. 1 of
        0 -> shiftR x 1
        1 -> shiftR x 1 `xor` cA
      j'   = case k - (cN - cM) of
        x | x < 0 -> x + cN
        x         -> x
      x'   = (arr ! j') `xor` xA
      arr' = arr // [(k, x')]
      k'   = case k + 1 of
        x | x > cN -> 0
        x          -> x
      y    = x' `xor` shiftR x' cU
      y'   = y `xor` (shiftL y cS .&. cB)
      y''  = y' `xor` (shiftL y' cT .&. cC)
      z    = y'' `xor` shiftR y'' cL
  in  (z, MTState arr' k')

mtGetValue :: Word32 -> Word32 -> Word32
mtGetValue seed exclusiveMax =
  let gen    = mkMTState seed
      (r, _) = mtNext gen
  in  r `mod` exclusiveMax
