factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial (x-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x,y) (p,q) = (x+p,y+q)

first :: (a,b,c) -> a
first (a, _, _) = a

second :: (a,b,c) -> b
second (_, b, _) = b

--head' :: [a] -> a
--head' [] = error "Can't get head of an empty list";
--head' (x:_) = x

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

-- guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal, but I bet you're ugly!"
    | bmi <= fat = "Yuo're fat. Lose some weight fatty!"
    | otherwise = "You're a whale. Congratulations!"
    where bmi = weight / height^2
          (skinny, normal, fat) = (18.5, 25.0, 20.0)

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
    | a == b = EQ
    | a > b = GT
    | otherwise = LT

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ ". "
    where (f:_) = firstName
          (l:_) = lastName

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi x y | (x, y) <- xs]
    where bmi height weight = height / weight^2

-- using let
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let pii = pi
        sideArea = 2 * pii * r * h
        topArea = pii * r^2
    in sideArea + 2*topArea

calcBmis' :: RealFloat a => [(a,a)] -> [a]
calcBmis' xs = [ bmi | (w,h) <- xs, let bmi = w/h^2]

head' :: [a] -> a
head' xs = case xs of [] -> error "Can't take head of an empty list"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ 
    case xs of [] -> "empty."
               [x] -> "a singleton list."
               otherwise -> " a long list."

