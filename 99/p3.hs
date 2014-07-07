--(*) Find the K'th element of a list. The first element in the list is number 1.

findKth :: (Num b, Eq b) => [a] -> b -> a
findKth [] _ = error "invalid index"
findKth (x:xs) k = if (k == 1)
                      then x
                   else
                      findKth xs (k-1)

--test cases
--  error
--    findKth [] 0
--    findKth [] 1
--    findKth [1] 2

--  success
--    findKth [1] 1
--    findKth [1,2,3] 2
--    findKth [1,2,3] 3
