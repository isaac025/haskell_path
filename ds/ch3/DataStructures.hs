{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module DataStructures (Heap) where

class Heap h where
    empty :: Ord a => h a
    isEmpty :: Ord a => h a -> Bool
    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a
    findMin :: Ord a => h a -> Maybe a
    deleteMin :: Ord a => h a -> Maybe (h a)

data LeftistHeap a = E
                   | T Int a (LeftistHeap a) (LeftistHeap a)
                   deriving (Read, Show, Eq)

instance Heap LeftistHeap where
    empty = E
    isEmpty h = h == empty
    
    merge h E = h
    merge E h = h
    merge h1@(T _ x l1 r1) h2@(T _ y l2 r2)
        | x <= y    = makeT x l1 (merge r1 h2)
        | otherwise = makeT y l2 (merge h1 r2)

    -- Exercise 3.2
    insert x E = T 1 x E E
    insert x h@(T r y a b)
        | x <= y = makeT x a h
        | otherwise = makeT x h b
       
    --insert x h = merge (T 1 x E E) h

    findMin E = Nothing
    findMin (T _ x l r) = Just x
    deleteMin E = Nothing
    deleteMin (T _ x l r) = Just (merge l r)


rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x h1 h2 
    | rank h1 >= rank h2    = T (rank h2 + 1) x h1 h2
    | otherwise             = T (rank h1 + 1) x h2 h1 

fromList :: Ord a => [a] -> LeftistHeap a
fromList xs = foldl (\acc x -> merge acc x) E xs'
    where xs' = map (\x -> insert x E) xs
