{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DataStructures (Set (..)
                      ) where

class Set s a where
    empty :: s a
    insert :: a -> s a -> s a
    member :: a -> s a -> Bool

data Tree a = E
            | Tree a (Tree a) (Tree a) 
            deriving (Show, Eq, Read)

instance Ord a => Set Tree a where
    empty = E

    insert x E = Tree x E E
    insert x t@(Tree e l r)
        | x == e = t
        | x < e = Tree e (insert x l) r
        | otherwise = Tree e l (insert x r)

    member x E = False
    member x (Tree e l r)
        | x == e = True
        | x < e = member x l
        | otherwise = member x r

-- Exercise 2.6:
class FiniteMap m k a where
    isEmpty :: m k a
    bind :: k -> a -> m k a -> m k a
    lookup :: k -> m k a -> Maybe a

data TMap k a = Empty
          | TMap (k,a) (TMap k a) (TMap k a)
          deriving (Show, Read, Eq)

instance Ord k => FiniteMap TMap k a where
    isEmpty = Empty

    lookup k Empty = Nothing
    lookup k t = lookup' t k t
        where lookup' (TMap (k,a) _ _) x Empty = if k == x then Just a else Nothing
              lookup' t x t'@(TMap (k,a) l r)
                | x <= k = lookup' t' x l
                | otherwise = lookup' t x r

    bind k a Empty = TMap (k,a) Empty Empty
    bind k a t@(TMap (k',a') l r)
        | k == k' = TMap (k,a) l r
        | k < k' = TMap (k',a') (bind k a l) r
        | otherwise = TMap (k',a') l (bind k a r) 
