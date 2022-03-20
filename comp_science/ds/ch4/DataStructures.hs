{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DataStructures where

class Stream s where
    (+|+) :: (Ord a) => s a -> s a -> s a
    take' :: (Ord a) => Int -> s a -> s a
    drop' :: (Ord a) => Int -> s a -> s a
    reverse' :: (Ord a) => s a -> s a

data StreamCell a = Nil
                  | Cons a (StreamCell a)
                  deriving (Show, Eq)

instance Stream StreamCell where
    Nil +|+ t = t
    Cons x s +|+ t = Cons x (s +|+ t)

    take' 0 s = Nil
    take' n Nil = Nil
    take' n (Cons x s) = Cons x (take' (n-1) s)

    -- Exercise 4.1
    drop' n s = let drop'' 0 s = s
                    drop'' n Nil = Nil
                    drop'' n (Cons x s') = drop'' (n-1) s'
               in drop'' n s

    --drop' 0 s = s
    --drop' n Nil = Nil
    --drop' n (Cons x s) = drop' (n-1) s
    
    reverse' s = let reverse'' Nil r = r
                     reverse'' (Cons x s) r = reverse'' s (Cons x r)
                 in reverse'' s Nil
    
