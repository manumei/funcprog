isInterleaving :: (Eq a) => [a] -> [a] -> [a] -> Bool
isInterleaving [] ys zs = ys == zs
isInterleaving xs [] zs = xs == zs
isInterleaving (_ : _) (_ : _) [] = False
isInterleaving (x : xs) (y : ys) (z : zs) = (x == z && isInterleaving xs (y : ys) zs) || (y == z && isInterleaving (x : xs) ys zs)