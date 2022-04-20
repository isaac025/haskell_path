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

data DoubleEndedQueue a = DoubleList [a] [a] deriving (Show, Eq)
instance Deque DoubleEndedQueue where
    empty' = DoubleList [] []
    isEmpty' (DoubleList f r) = null f && null r
    
    cons x (DoubleList [] []) = DoubleList [x] []
    cons x (DoubleList f r) = checkf' (x:f) r
    top' (DoubleList [] _) = Nothing
    top' (DoubleList (x:f) _) = Just x
    pop' (DoubleList (x:f) r) = case isEmpty' q' of
        True -> Nothing
        False -> Just q'
        where q' = checkf' f r

    push' (DoubleList f r) x = checkf' f (x:r)
    peek (DoubleList _ []) = Nothing
    peek (DoubleList _ (x:r)) = Just x
    eject (DoubleList f (x:r)) = case isEmpty' q' of
        True -> Nothing
        False -> Just q'
        where q' = checkf' f r
    
checkf' :: (Ord a) => [a] -> [a] -> DoubleEndedQueue a
checkf' [] r = DoubleList f r''
    where f = fst $ splitAt half r'
          r'' = reverse $ snd $ splitAt half r'
          half = length r `div` 2
          r' = reverse r
checkf' f [] = DoubleList f r'
    where f = fst $ splitAt half f
          r' = reverse $ snd $ splitAt half f
          half = length f `div` 2
checkf' f r = DoubleList f r

fromList' :: (Ord a) => [a] -> DoubleEndedQueue a
fromList' [] = DoubleList [] []
fromList' ls = foldl (\acc x -> push' acc x) (DoubleList [] []) ls

-- Class for Heap 
class Heap h where
    empty'' :: (Ord a) => h a
    isEmpty'' :: (Ord a) => h a -> Bool
    insert :: (Ord a) => a -> h a -> h a
    merge :: (Ord a) => h a -> h a -> h a
    findMin :: (Ord a) => h a -> Maybe a
    deleteMin :: (Ord a) => h a -> Maybe (h a)

data SplayHeap a = E | Tree (SplayHeap a) a (SplayHeap a) deriving (Show, Eq)
instance Heap SplayHeap where
    empty'' = E
    isEmpty'' E = True
    isEmpty'' _ = False

    insert x t = Tree (smaller x t) x (bigger x t)

bigger pivot E = E
bigger pivot (Tree a x b) = if x <= pivot then bigger pivot b 
    else case a of
            E -> (Tree E x b) 
            (Tree a' y a'') -> if y <= pivot then (Tree (bigger pivot a'') x b)
                              else (Tree (bigger pivot a') y (Tree a'' x b))
smaller pivot E = E
smaller pivot (Tree a x b) = if x > pivot then smaller pivot a
    else case b of
            E -> (Tree a x E)
            (Tree b' y b'') -> if y > pivot then (Tree x a (smaller pivot b'))
                               else (Tree ( ) y ( ))
