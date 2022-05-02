module Exercises where

import DataStructures

fromList :: (Ord a) => [a] -> StreamCell a
fromList n = foldl (\acc x -> Cons x acc) Nil n

-- 4.2
insertionSort :: (Ord a) => StreamCell a -> StreamCell a
insertionSort Nil = Nil
insertionSort (Cons x Nil) = (Cons x Nil)
insertionSort (Cons x xs) = insert $ insertionSort xs
    where insert Nil = (Cons x Nil)
          insert (Cons y ys)
            | x < y = Cons x (Cons y ys)
            | otherwise = Cons y (insert ys)
