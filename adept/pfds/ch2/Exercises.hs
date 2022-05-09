{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import DataStructures

{- Exercise 2.1:
 - write a function suffixes that takes a list
 - and returs a list of all the suffixes in decreasing order
 - e.g. suffixes [1,2,3,4] = [[1,2,3,4],[2,3,4],[3,4],[4],[]]
 -}

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : suffixes (tail xs)

data Tree a = E
            | Tree a (Tree a) (Tree a) 
            deriving (Show, Eq, Read)

instance Ord a => Set Tree a where
    empty = E

-- Exercise 2.2
    member x E = False
    member x t@(Tree e _ _) = member' e x t
        where member' c x E = x == c
              member' c x (Tree e l r)
                | x <= e = member' e x l
                | x > e = member' c x r

-- Exercise 2.3 and 2.4
    insert x t = if not (member x t) then insert' x t else t
        where insert' x E = Tree x E E
              insert' x t@(Tree e l r)
                | x < e = Tree e (insert' x l) r
                | otherwise = Tree e l (insert' x r)

-- Exercise 2.5 (a)
complete :: a -> Int -> Tree a
complete x 0 = Tree x E E
complete x d = Tree x (complete x (d-1)) (complete x (d-1))

-- Exercise 2.5 (b)

