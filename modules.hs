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

myIterate :: (a -> a) -> a -> [a]
myIterate fc x = x : myIterate fc (fc x)

mySplitAt :: Int -> [a] -> ([a],[a])
myTake' _ [] = []
myTake' n (x:xs) = if (n <= 0) then []
                   else (x:myTake' (n-1) xs)
myDrop' _ [] = []
myDrop' n (x:xs) = if (n <= 0) then (x:xs)
                   else (myDrop' (n-1) xs)
mySplitAt n xs = ((myTake' n xs), (myDrop' n xs))

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile fc (x:xs) = if (fc x) then myDropWhile fc xs
                        else (x:xs)

myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits (x:xs) = [] : (map (x:) $ myInits xs)

myTails :: [a] -> [[a]]
myTails xs = xs : case xs of
                    [] -> []
                    _:xs -> myTails xs

-- searching for a sublist in a list
search :: (Eq a) => [a] -> [a] -> Bool
search pattern src = 
    let len = length pattern
    in (foldl (\acc x -> if (take len x == pattern) then True
                         else acc) False (myTails src))

myPartition :: (a -> Bool) -> [a] -> ([a],[a])
getHalf fc xs = case xs of
                    [] -> []
                    (y:ys) -> if (fc y) then (y:getHalf fc ys)
                             else (getHalf fc ys)
myPartition fc xs = ((getHalf fc xs), (getHalf (not.fc) xs))
