-- logic:
-- I can start building up from bottom row, and on every higher row, mentally replace its values with each value plus the maximum of its two children
-- do recursion, keeping the maxes, until top row which is just 1 element so it will be the max max

maximumPyramidSum :: [[Integer]] -> Integer
maximumPyramidSum rows = head (bestSums rows) -- top row ends as one number: the answer
  where
    bestSums [bottomRow] = bottomRow -- bottom row: best sum is the value itself (single list case, otherwise go pattern matching for multi-row lists)
    bestSums (row : rowsBelow) = addBestChild row (bestSums rowsBelow) -- solve the rows below first, then this row

    -- addBestCHild takes a row and the best sums of the row below, and returns the best sums for this row
    -- if [a,b,c], [w,x,y,z], then it starts with a and max(w,z)
    -- then moves on to b and max(x,y), and so on for the rest of the row
    -- until the current row has no values left, then it doesnt go into original call
    -- it goes and pattern-matches into the _ _ case and gives an empty list, so the stuff just gets concatenated on it
    addBestChild (value : restOfRow) (leftChild : rightChild : restBelow) = best : rest -- this value's result, then the rest of the row's
      where
        best = value + max leftChild rightChild -- value plus bigger child
        rest = addBestChild restOfRow (rightChild : restBelow) -- same for the next value (iterate right until end)
    addBestChild _ _ = [] -- no values left: row done