module DataStructures where

-- Class Queue 
class Queue q where
   empty :: (Ord a) => q a
   isEmpty :: (Ord a) => q a -> Bool
   push :: (Ord a) => q a -> a -> q a
   top :: (Ord a) => q a -> Maybe a
   pop :: (Ord a) => q a -> Maybe (q a)
   
-- Data Structure Batched Queue
data BatchedQueue a = BatchedQueue [a] [a] deriving (Show, Eq)

instance Queue BatchedQueue where
    empty =  BatchedQueue [] []     
    isEmpty (BatchedQueue f _) = null f

    push (BatchedQueue f r) x = checkf f (x : r)

    top (BatchedQueue [] _) = Nothing
    top (BatchedQueue (x:f) _) = Just x

    pop (BatchedQueue (x:f) r) = case isEmpty q' of
        True -> Nothing 
        False -> Just q'
        where q' = checkf f r

checkf :: (Ord a) => [a] -> [a] -> BatchedQueue a
checkf [] r = BatchedQueue (reverse r) []
checkf f r = BatchedQueue f r

fromList :: (Ord a) => [a] -> BatchedQueue a
fromList [] = BatchedQueue [] []
fromList ls = foldl (\acc x -> push acc x) (BatchedQueue [] []) ls 

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

checkf' :: (Ord a) => Deque a -> Dequ a
checkf' 
