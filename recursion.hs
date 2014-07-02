maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum' can't be called on empty list."
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Ord a, Integral a) => a -> b -> [b]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x
