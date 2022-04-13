module DataStructures where

-- Class Queue 
class Queue q where
   empty :: (Ord a) => q a
   isEmpty :: (Ord a) => q a -> Bool
   push :: (Ord a) => q a -> a -> q a
   top :: (Ord a) => q a -> Maybe a
   pop :: (Ord a) => q a -> Maybe (q a)
   
-- Data Structure Batched Queue
data BatchedQueue a = List [a] [a]

instance Queue BatchedQueue where
    empty = List [] []     
    isEmpty (List f _) = null f

    push (List [] _) x = List [x] []
    push (List f r) x = List f (x:r)

    top (List [] _) = Nothing
    top (List (x:f) _) = Just x

    pop (List [] _) = Nothing
    pop (List (x:f) r) = Just (List f r)

checkf :: (Ord a) => BatchedQueue a -> BatchedQueue a
checkf (List [] r) = (List (reverse r) [])
checkf q = q

-- Exercise 5.1
class Deque q where
    empty' :: (Ord a) => q a
    isEmpty' :: (Ord a) => q a -> Bool

    cons :: (Ord a) => a -> q a -> q a
    top' :: (Ord a) => q a -> Maybe a
    pop' :: (Ord a) => q a -> Maybe (q a)

    push' :: (Ord a) => q a -> a -> q a
    peek :: (Ord a) => q a -> Maybe a
    eject :: (Ord a) => q a -> Maybe (q a)

data DoubleEndedQueue a = DoubleList [a] [a]

{- Function that makes the list symmetric
 - for pop and eject
 -}
instance Deque DoubleEndedQueue where
    empty' = DoubleList [] []
    isEmpty' (DoubleList f r) = null f && null r
    
    cons x (DoubleList [] []) = (DoubleList [x] [])
    cons x (DoubleList f []) = (DoubleList f [x])
    cons x (DoubleList [] r) = (DoubleList f' r')
        where f' = take halfOfR r
              halfOfR = (length r) `div` 2
              r' = x : drop halfOfR r
    cons x (DoubleList f r) = DoubleList f (x:r)

    top' (DoubleList [] _) = Nothing
    top' (DoubleList (x:f) _) = Just x

    pop' (DoubleList [] []) = Nothing
    pop' (DoubleList _ xs) = Just (DoubleList xs)


