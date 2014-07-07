--(*) Find the number of elements of a list.

  myLength :: (Num b) => [a] -> b
  myLength xs = foldl (\acc _ -> acc+1) 0 xs