-- Manuel Meirino, S6856349

digits :: Integer -> [Integer]
digits n
  | n < 10 = [n]
  | otherwise = n `mod` 10 : digits (n `div` 10)

reductionLength :: Integer -> Int
reductionLength n = go 1 n
  where
    go :: Int -> Integer -> Int
    go step m
      | m < 10 = 0
      | odd step = 1 + go (step + 1) (product (digits m))
      | otherwise = 1 + go (step + 1) (sum (digits m))