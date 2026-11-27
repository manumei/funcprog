-- keep two variables
-- plus:  current max sum for the best segment up until x, where x is ADDED (even index relative to the segment)
-- minus: current max sum for the best segment up until x, where x is SUBTRACTED (odd index relative to the segment)
-- tracker function receives params tracker(list, plus, minus, best)
-- when adding x, always check if the current leader segment is bigger than zero or not, since if it's negative, then it will be better to dump and start a new segment instead
-- update best on each step as the max between best and plus, since minus will always make the segment smaller (non-negative elems)

maximumZigzagSum :: [Integer] -> Integer
maximumZigzagSum (a : as) = tracker as a 0 a -- base case, first element is added, nothing subtracted (minus=0)
  where
    tracker :: [Integer] -> Integer -> Integer -> Integer -> Integer
    tracker [] _ _ best = best
    tracker (x : xs) plus minus best = tracker xs plus' minus' (max best plus') -- updated values, x is current, xs is rest of list
      where
        plus' = x + max 0 minus -- x added: if the last segment was negative, then start fresh, if it was bigger than zero, continue it
        minus' = plus - x -- x subtracted: extend segment that ended with addition, plus was already the best so nothing else to do