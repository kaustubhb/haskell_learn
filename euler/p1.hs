--If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
--The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 below 1000.

multiplesOf3or5Below :: (Integral a) => a -> a
multiplesOf3or5Below n = sum [x | x <- [3,4..(n-1)], or [(x `mod` 3 == 0), (x `mod` 5 == 0)]]