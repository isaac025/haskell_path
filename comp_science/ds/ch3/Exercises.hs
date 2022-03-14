module Exercises where

import DataStructures

-- Exercise 3.3
fromList :: Ord a => [a] -> LeftistHeap a
fromList xs = foldl (\acc x -> merge acc x) ELH xs'
    where xs' = map (\x -> insert x ELH) xs

fromList' :: Ord a => [a] -> WeightBiasedLeftistHeap a
fromList' xs = foldl (\acc x -> merge acc x) EWBLH xs'
    where xs' = map (\x -> insert x EWBLH) xs

