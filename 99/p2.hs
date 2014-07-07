--(*) Find the last but one element of a list.

secondLast :: [a] -> a
secondLast xs = if length xs >= 2
                  then last (init xs)
                else error "Too few parameters"

secondLast' [] = error "Empty List"
secondLast' [x] = error "Too few parameters"
secondLast' xs = xs !! (length xs - 2)