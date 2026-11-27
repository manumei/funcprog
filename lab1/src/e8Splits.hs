-- math logic
-- lets say, for a given ditribution of splits, k is the amount of segments and s is the sum of each segment
-- then both k and s have to be divisors of the total sum, cause you need k*s=totalSum
-- length <= totalSum cause list is only positive integers, so more efficient to iterate through the length
-- k is upper bounded by length since there cannot be more segments than the length of the list
-- so, from k=1 to k=length, try totalSum%k==0, this cuts most cases,
-- and to also save half the time, dont look above length/2 since the only option for that is that k==length (all elements are the same)
-- and I implemented allEquals to check this, which gives 1 if all elements are equal (1 extra split), 0 otherwise.

countValidSplits :: [Integer] -> Integer
-- for every k from 1 to n, check first that it's a divisor of the sum (drops most cases, so big lists dont get big recursion)
-- then check if it actually works as a segment, if it does then append it to the list
countValidSplits xs = toInteger (length [k | k <- [1 .. n], total `mod` k == 0, splitsInto (total `div` k) xs])
  where
    total = sum xs -- sum of the whole list
    n = toInteger (length xs) -- maximum number of segmen

splitsInto :: Integer -> [Integer] -> Bool
-- can the list be cut into segments that each sum to exactly targetSum?
splitsInto targetSum list = walk 0 list -- start with a running total of 0
  where
    walk runningTotal [] = runningTotal == 0 -- list finished: OK only if the last segment was completed
    walk runningTotal (current : rest) -- take the next element (current) and dive in
      | runningTotal + current == targetSum = walk 0 rest -- segment complete: cut, and start the next one from 0
      | runningTotal + current < targetSum = walk (runningTotal + current) rest -- not there yet: keep adding
      | otherwise = False -- jumped past targetSum: impossible, cause elements are positive
