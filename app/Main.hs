{-# LANGUAGE OverloadedStrings #-}
-- ~ {-# LANGUAGE FlexibleContexts #-}
-- ~ {-# LANGUAGE AllowAmbiguousTypes #-}

-- | A miscellany of sketches, purely self-educational.
module Main where

import Lib


-- | Insertion sort
--
-- Examples:
--
-- >>> foldR inserTT "" "the quick brown fox jumps over the lazy dog"
-- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
inserTT :: Ord a => a -> [a] -> [a]
inserTT x []      = [x]
inserTT x (y:ys)
  | x <= y        = x:y:ys
  | otherwise     = y:(inserTT x ys)


-- | Canonical quick sort
--
-- Examples:
--
-- >>> quickSort "the quick brown fox jumps over the lazy dog"
-- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]


{- Folding -}


-- | If the list is empty, the result is the initial value z; else
-- apply f to the first element and the result of folding the rest.
--
-- Examples:
--
-- >>> foldR (:) [11,12,13] [1,2]
-- [1,2,11,12,13]
-- >>> (flip $ foldR (:)) [1,2] [11,12,13]
-- [1,2,11,12,13]
foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f z []     = z 
foldR f z (x:xs) = f x (foldR f z xs) 
 

-- | If the list is empty, the result is the initial value; else
-- we recurse immediately, making the new initial value the result
-- of combining the old initial value with the first element.
--
-- Examples:
--
-- >>> foldL (\xs x -> xs ++ [x]) [1,2] [11,12,13]
-- [1,2,11,12,13]
-- >>> foldL ((. return) . (++)) [1,2] [11,12,13]
-- [1,2,11,12,13]
foldL :: (a -> b -> a) -> a -> [b] -> a
foldL f z []     = z                
foldL f z (x:xs) = foldL f (f z x) xs


-- Reduction to poinfree
a = \xs x -> xs ++ [x]
b = \xs x -> xs ++ return x
c = \xs x -> ((xs ++) . return) x
d = \xs x -> ((. return) (xs ++)) x
e = \xs x -> ((. return) . (++)) xs x


-- | foldL with no starting value argument.
--
-- Examples:
--
-- >>> foldL1 (+) [1,2,3]
-- 6
foldL1 :: (a -> a -> a) -> [a] -> a
foldL1 f (x:xs) = foldL f x xs


-- | foldR with no starting value argument, recursive.
--
-- Examples:
--
-- >>> foldR1 (+) [1,2,3]
-- 6
foldR1 :: (a -> a -> a) -> [a] -> a
foldR1 f [x]    = x
foldR1 f (x:xs) = f x (foldR1 f xs)


-- | foldR with no starting value argument.
--
-- Examples:
--
-- >>> foldR1' (+) [1,2,3]
-- 6
foldR1' :: (a -> a -> a) -> [a] -> a
foldR1' f (x:xs) = foldR f x xs


{- Mapping and filtering -}


-- | Map, recursive.
--
-- Examples:
--
-- >>> maP (+10) [1,2,3]
-- [11,12,13]
maP :: (a -> b) -> [a] -> [b]
maP _ []     = []
maP f (x:xs) = f x : maP f xs


-- | Filter, recursive.
--
-- Example:
--
-- >>> filter (>0) [-2,11,0,43]
-- [11,43]
filteR :: (a -> Bool) -> [a] -> [a]
filteR _ []     = []
filteR p (x:xs)
  | p x         = x : filteR p xs
  | otherwise   =     filteR p xs


-- | Map, by foldr.
--
-- Examples:
--
-- >>> maPR (+10) [1,2,3]
-- [11,12,13]
maPR :: (a -> b) -> [a] -> [b]
maPR f = foldR (\x acc -> f x : acc) []

-- | Filter, by foldr.
--
-- Example:
--
-- >>> filter (>0) [-2,11,0,43]
-- [11,43]
filteRR :: (a -> Bool) -> [a] -> [a]
filteRR p = foldR (\x acc -> if p x then x : acc else acc) []


main :: IO ()
main = someFunc
