factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x,y) (p,q) = (x+p,y+q)

first :: (a,b,c) -> a
first (a, _, _) = a

second :: (a,b,c) -> b
second (_, b, _) = b

head' :: [a] -> a
head' [] = error "Can't get head of an empty list";
head' (x:_) = x

length' :: (Num a) => [b] -> a
length' [] = 0
length' (_:xs) = 1 + (length' xs)

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- patterns
capital :: String -> String
capital "" = "Empty String, whoops!"
capital whole@(x:xs) = "The first letter of " ++ whole ++ " is " ++ [x]
