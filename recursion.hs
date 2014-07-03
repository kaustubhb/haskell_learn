maximum' :: Ord a => [a] -> a
maximum' [] = error "maximum' can't be called on empty list."
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: (Ord a, Integral a) => a -> b -> [b]
replicate' n x
    | n <= 0 = []
    | otherwise = x : replicate' (n-1) x

take' :: Int-> [b] -> [b]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x