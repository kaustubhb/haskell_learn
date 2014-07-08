import Data.List

myNub :: (Eq a) => [a] -> [a]
myNub [] = []
myNub xs = reverse $ foldl (\acc x -> if (x `elem` acc)
                              then acc
                            else x:acc) [head xs] xs

myNub2 :: (Eq a) => [a] -> [a]
myNub2 xs = myNub2' xs []
  where
    myNub2' [] _ = []
    myNub2' (y:ys) ls
      | y `elem` ls = myNub2' ys ls
      | otherwise = y : (myNub2' ys ls)

myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse i xs = init $ foldr (\x acc -> x:i:acc) [] xs

myIntercalate :: [a] -> [[a]] -> [a]
myIntercalate ys xss = foldr1 (\x acc -> x ++ ys ++ acc) xss

myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose ([]:xss) = myTranspose xss
myTranspose ((x:xs):xss) = (x : [h | (h:_) <- xss]): (myTranspose $ xs : [ys | (_:ys) <- xss])

myConcat :: [[a]] -> [a]
myConcat xss = foldr (++) [] xss

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny fc (x:xs)
    | (fc x) == True = True
    | otherwise = myAny fc xs

-- Not working as of now
myIterate :: (a -> a) -> a -> [a]
myIterate fc x = myIterate' fc x []
  where
    myIterate' fc y ys = myIterate' fc h (ys ++ [h])
      where h = (fc y)
