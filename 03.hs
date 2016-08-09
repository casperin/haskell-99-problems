import Data.List
import Data.Ord (comparing)

-- 21:
-- Insert an element at a given position into a list.

split (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
split xs _             = ([], xs)

insertAt _ xs n | n < 1 = xs
insertAt _ xs n | (n-1) > length xs = xs
insertAt x xs n = first ++ [x] ++ last
  where (first, last) = split xs (n-1)


-- 22:
-- Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range n m | n > m = []
range n m = n : range (n+1) m


-- 23 - 25:
-- All deals with random which I'll skip for now


-- 26:
-- Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people?
-- We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
-- well-known binomial coefficients). For pure mathematicians, this result may
-- be great. But we want to really generate all the possibilities in a list.

combinations 0 _  = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

-- 27:
-- Problem is above the scope for now.
-- https://wiki.haskell.org/99_questions/21_to_28#Problem_27


-- 28:
-- Sorting a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort xs = sortBy (comparing $ sortHelper) xs
  where sortHelper x = length $ filter (\y -> length y == length x) xs
