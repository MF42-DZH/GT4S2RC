module S2RA.Maths where

bezout :: Integral a => a -> a -> (a, a)
bezout _ 0 = (1, 0)
bezout a b =
  let (q, r) = a `divMod` b
      (u, v) = bezout b r
  in  (v, u - (q * v))

coprime :: Integral a => a -> a -> Bool
coprime = ((== 1) .) . gcd

modInverse :: Integral a => a -> a -> a
modInverse mult modulus
  | coprime mult modulus = ((x `mod` modulus) + modulus) `mod` modulus
  | otherwise            = error "Unsolveable. Inverse does not exist as the multiplier and modulus are not coprime."
  where (x, _) = bezout mult modulus
