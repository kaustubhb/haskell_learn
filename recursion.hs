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

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
    | y == x = True
    | otherwise = elem' y xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = let
      smallerList = [a | a <- xs, a < x]
      biggerList = [a | a <- xs, a >= x]
      in (qsort smallerList) ++ [x] ++ (qsort biggerList)
