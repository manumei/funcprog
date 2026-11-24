-- Manuel Meirino, S6856349

accumulatedPowers :: Integer -> Integer -> Integer -> Integer
accumulatedPowers _ 0 m = 1 `mod` m
accumulatedPowers a n m
  | odd n = (s + s * powMod a (k + 1) m) `mod` m
  | otherwise = (accumulatedPowers a (n - 1) m + powMod a n m) `mod` m -- add up biggest, then do odd-case splitting on remains (recursion cause n-1) will be odd if n even
  where
    k = n `div` 2
    s = accumulatedPowers a k m

powMod :: Integer -> Integer -> Integer -> Integer
powMod a e m
  | e == 0 = 1 `mod` m
  | even e = (half * half) `mod` m
  | otherwise = (a * half * half) `mod` m
  where
    half = powMod a (e `div` 2) m