-- p1
--(*) Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "Empty List"
myLast xs = foldr1 (\_ acc -> acc) xs

myLast' [] = error "Empty list"
myLast' [x] = x
myLast' (x:xs) = myLast' xs

-- p2
--(*) Find the last but one element of a list.

secondLast :: [a] -> a
secondLast xs = if length xs >= 2
                  then last (init xs)
                else error "Too few parameters"

secondLast' [] = error "Empty List"
secondLast' [x] = error "Too few parameters"
secondLast' xs = xs !! (length xs - 2)

-- p3
--(*) Find the K'th element of a list. The first element in the list is number 1.

findKth :: (Num b, Eq b) => [a] -> b -> a
findKth [] _ = error "invalid index"
findKth (x:xs) k = if (k == 1)
                      then x
                   else
                      findKth xs (k-1)

-- p4
--(*) Find the number of elements of a list.

myLength :: (Num b) => [a] -> b
myLength xs = foldl (\acc _ -> acc+1) 0 xs

-- p5
--(*) Reverse a list. 

myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs

-- p6
--(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

--p7
--(**) Flatten a nested list structure. 
-- TODO later with more knowledge




--p8 
--(**) Eliminate consecutive duplicates of list elements.
--If a list contains repeated elements they should be replaced with a single copy of the element. 
--The order of the elements should not be changed. 

compress :: (Eq a) =>  [a] -> [a]
compress [] = []
compress xs = foldr (\x acc -> if ((head acc) == x) 
                                  then acc 
                                else (x:acc)) [last xs] xs