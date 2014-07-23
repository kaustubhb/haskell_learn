toDigits :: (Integral a) => a -> [a]
toDigits x
  | x == 0 = []
  | otherwise = (x `mod` 10) : toDigits (x `div` 10)