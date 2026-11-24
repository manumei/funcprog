-- Manuel Meirino, S6856349

isPowerful :: Integer -> Bool
isPowerful n = any isPowerOf bases
  where
    bases = takeWhile smallEnough [2 ..]
    isPowerOf a = n `elem` takeWhile (<= n) (iterate (* a) (a * a))
    smallEnough a = a * a <= n -- bool (given a, check if a*a <= n)