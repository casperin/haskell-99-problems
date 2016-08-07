-- https://wiki.haskell.org/99_questions/1_to_10
ns = [1..10]
cs = ['a'..'z']
str = "Hello, World!"

-- 1: Find the last element of a list.
myLast = head . reverse

-- 2: Find the last but one element of a list.
myButLast = myLast . init

-- 3: Find the last but one element of a list.
elementAt xs k = xs !! (k - 1)

-- 4: Find the number of elements of a list.
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 5: Reverse a list.
-- myReverse [] = []
-- myReverse (x:xs) = myReverse xs ++ [x]
myReverse list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

-- 6: Find out whether a list is a palindrome. A palindrome can be read
-- forward or backward; e.g. (x a m a x).
-- isPalindrome [] = True
-- isPalindrome (x:[]) = True
-- isPalindrome (x:xs) = x == last xs && (isPalindrome $ init xs)
isPalindrome xs = xs == (reverse xs)

-- 7: Flatten a nested list structure.
-- WAT
data NestedList a = Elem a | List [NestedList a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- 8: Eliminate consecutive duplicates of list elements.
-- compress = map head . group

-- compress []     = []
-- compress (x:xs) = x : (compress $ dropWhile (== x) xs)

compress xs = compress' xs []
  where
    compress' [] compressed = compressed
    compress' (x:xs) compressed =
      if (elem x compressed)
        then compress' xs compressed
        else compress' xs (compressed ++ [x])

-- 9: Pack consecutive duplicates of list elements into sublists. If a
-- list contains repeated elements they should be placed in separate
-- sublists.
pack (x:xs) = let (first,rest) = span (==x) xs
               in (x:first) : pack rest
pack [] = []

-- 10: Run-length encoding of a list. Use the result of problem P09 to
-- implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is
-- the number of duplicates of the element E.

-- encode xs = map (\x -> (length x,head x)) $ pack xs

-- encode xs = [(length x, head x) | x <- pack xs]

encode xs = map getTuple $ pack xs
  where getTuple x = (length x, head x)
