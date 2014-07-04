applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' func (x:xs) (y:ys) = 
    (func x y):(zipWith' func xs ys)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g y x = f x y