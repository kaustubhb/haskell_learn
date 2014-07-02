doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

conanO'Brien = "Its a-me, ConanO'Brien!"

boomBangs xs = [if x < 10 then "BOOM" else "BANG" | x <- xs, odd x ]

length' xs = sum[1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase xs = [x | x <- xs, x `elem` ['A'..'Z']]

-- call with [[1,2,3,4],[5,6,7,8],[9,10]]

removeOdds :: [[Int]] -> [[Int]]
removeOdds xxs = [[x | x <- xs, even x] 
                   | xs <- xxs]

--let triangles = [(x,y,z) | x <- [1..10], y <- [1..x], z <- [1..y],  y*y + z*z == x*x]

factorial :: Integer -> Integer
factorial x = product [1..x]