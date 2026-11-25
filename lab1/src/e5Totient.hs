import Data.List (group)

totient :: Integer -> Integer
totient n = product [p ^ (e - 1) * (p - 1) | (p, e) <- peFactors n]

peFactors :: Integer -> [(Integer, Int)]
peFactors n = [(head g, length g) | g <- group (primeFactors n)]

-- e.g. primeFactors 12 == [2,2,3]
primeFactors :: Integer -> [Integer]
primeFactors n = loop 2 n
  where
    loop k m
      | m == 1 = []
      | k * k > m = [m]
      | m `mod` k == 0 = k : loop k (m `div` k)
      | otherwise = loop (k + 1) m
