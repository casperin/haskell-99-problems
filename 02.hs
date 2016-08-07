import Data.List

-- https://wiki.haskell.org/99_questions/2_to_20
ns = [1..10]
cs = ['a'..'z']
str = "Hello, World!"
str2 = "aaaabccaadeeee"


-- 11:
-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.

pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest

encode xs = [(length x, head x) | x <- pack xs]

data PackedListItem a = Single a | Multiple Int a
  deriving Show

encodeModified [] = []
encodeModified xs = map compress $ encode xs
  where
    compress (1, a) = Single a
    compress (n, a) = Multiple n a


-- 12:
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
encodedStr = encodeModified str2

decodeModified :: [PackedListItem a] -> [a]
decodeModified = foldr decodeModified' []
  where
    decodeModified' (Single a)     acc = a : acc
    decodeModified' (Multiple n a) acc = (replicate n a) ++ acc

-- Better version:
-- decodeModified = concatMap decodeHelper
--   where
--     decodeHelper (Single x)     = [x]
--     decodeHelper (Multiple n x) = replicate n x


-- 13:
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them. As in problem P11,
-- simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect xs = map pack' $ group xs
  where pack' [x] = Single x
        pack' xs  = Multiple (length xs) (head xs)


-- 14:
-- Duplicate the elements of a list.
dupli [] = []
dupli (x:xs) = x : x : dupli xs


-- 15:
-- Replicate the elements of a list a given number of times.
repli xs n = concatMap (\x -> replicate n x) xs


-- 16:
-- Drop every N'th element from a list.
dropEvery xs n = helper xs 1 n
  where
    helper [] _ _ = []
    helper (x:xs) i n = if mod i n == 0
      then helper xs (i + 1) n
      else x : helper xs (i + 1) n


-- 17:
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split xs n = helper xs n []
  where
    helper [] _ acc = (acc, [])
    helper (x:xs) n acc = if n > 0
      then helper xs (n-1) (acc ++ [x])
      else (acc, x:xs)

-- Better:
-- split :: [a] -> Int -> ([a], [a])
-- split (x:xs) n | n > 0 = let (f,l) = split xs (n-1) in (x : f, l)
-- split xs _             = ([], xs)


-- 18:
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.
slice (_:xs) i k | i > 1 = slice xs (i-1) (k-1)
slice xs _ k = take' xs k []
  where
    take' (x:xs) k acc | k > 0 = take' xs (k-1) (acc++[x])
    take' _ _ acc = acc


-- 19:
-- Rotate a list N places to the left.
rotate [] _ = []
rotate xs 0 = xs
rotate xs n = last ++ first
  where
    index = if n > 0 then n else length xs + n
    (first, last) = split xs index


-- 20:
-- Remove the K'th element from a list.
-- removeAt n xs | n > length xs || n < 0 = (Nothing, xs)
-- removeAt n xs = (Just $ xs !! (n-1), take (n-1) xs ++ drop n xs)
removeAt n xs
  | n > length xs = notFound
  | n < 1         = notFound
  | otherwise     = (Just $ xs !! (n-1), take (n-1) xs ++ drop n xs)
  where notFound = (Nothing, xs)
