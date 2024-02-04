import Data.List (union) -- This implementation uses union

-- All multiples of '[n]' up to 'a'
-- Implemented recursively
multiplesOfSmallerThan n a
    | length n /= 1 = union (multiplesOfFirstInListSmallerThan n a) (multiplesOfSmallerThan (tail n) a)
    | otherwise     =       (multiplesOfFirstInListSmallerThan n a)
    -- All multiples of the first element in [n] up to 'a'
    where multiplesOfFirstInListSmallerThan n a = [x | x <- [1..a-1], x `mod` head n == 0]

-- Sum of all multiples of 3 and 5 smaller than 1000
solution = sum (multiplesOfSmallerThan [3,5] 1000)