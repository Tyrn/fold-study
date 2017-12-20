-- | A miscellany of sketches, purely self-educational.
module Main where

import Lib

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


main :: IO ()
main = someFunc
