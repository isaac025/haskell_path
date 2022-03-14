{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module DataStructures where

-- Class for Heap 
class Heap h where
    empty :: Ord a => h a
    isEmpty :: Ord a => h a -> Bool
    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a
    findMin :: Ord a => h a -> Maybe a
    deleteMin :: Ord a => h a -> Maybe (h a)

-- Data Structure LeftistHeap
data LeftistHeap a = ELH
                   | TLH Int a (LeftistHeap a) (LeftistHeap a)
                   deriving (Read, Show, Eq)

instance Heap LeftistHeap where
    empty = ELH
    isEmpty h = h == empty
    
    merge h ELH = h
    merge ELH h = h
    merge h1@(TLH _ x l1 r1) h2@(TLH _ y l2 r2)
        | x <= y    = makeT x l1 (merge r1 h2)
        | otherwise = makeT y l2 (merge h1 r2)

    -- Exercise 3.2
    insert x ELH = TLH 1 x ELH ELH
    insert x h@(TLH r y a b)
        | x <= y = makeT x a h
        | otherwise = makeT x h b
    --insert x h = merge (T 1 x E E) h

    findMin ELH = Nothing
    findMin (TLH _ x l r) = Just x
    deleteMin ELH = Nothing
    deleteMin (TLH _ x l r) = Just (merge l r)

rank :: LeftistHeap a -> Int
rank ELH = 0
rank (TLH r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x h1 h2 
    | rank h1 >= rank h2    = TLH (rank h2 + 1) x h1 h2
    | otherwise             = TLH (rank h1 + 1) x h2 h1 

-- Data Structure WeightBiasedLeftistHeap
-- Exercise 3.4 (b)
data WeightBiasedLeftistHeap a = EWBLH 
                   | TWBLH Int a (WeightBiasedLeftistHeap a) (WeightBiasedLeftistHeap a)
                   deriving (Read, Show, Eq)

instance Heap WeightBiasedLeftistHeap where
    empty = EWBLH
    isEmpty w = w == empty

    -- Exercise 3.5 (c)
    -- modify merge to a single top-down pass
    merge w EWBLH = w
    merge EWBLH w = w
    merge w1@(TWBLH _ x l1 r1) w2@(TWBLH _ y l2 r2)
        | x <= y    = makeT' x l1 (merge r1 w2)
        | otherwise = makeT' y l2 (merge w1 r2)

    insert x w = merge (TWBLH 1 x EWBLH EWBLH) w
    findMin EWBLH = Nothing
    findMin (TWBLH _ x l r) = Just x
    deleteMin EWBLH = Nothing
    deleteMin (TWBLH _ x l r) = Just (merge l r)

size :: WeightBiasedLeftistHeap a -> Int
size EWBLH = 0
size (TWBLH _ _ l r) = 1 + (size l) + (size r)

makeT' x w1 w2
    | size w1 >= size w2    = TWBLH (size w2 + 1) x w1 w2
    | otherwise             = TWBLH (size w1 + 1) x w2 w1

-- Data Structure Tree & Binomial(Tree)Heap
data Tree a = E
            | Node Int a [Tree a] deriving (Show, Eq, Read)

newtype BinomialHeap a = BH [Tree a] deriving (Show, Eq, Read)

instance Heap BinomialHeap where
    empty = BH []
    isEmpty (BH ts) = null ts

    merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)
    insert x (BH ts) = BH (insTree (Node 0 x []) ts)

    findMin (BH ts) = root t
        where Just (t,ts) = removeMinTree ts

    deleteMin (BH ts) = Just (BH (mrg (reverse ts1) ts2))
        where Just ((Node _ x ts1), ts2) = removeMinTree ts

link t1@(Node r x1 c1) t2@(Node _ x2 c2)
    | x1 <= x2 = Node (r+1) x1 (t2:c1)
    | otherwise = Node (r+1) x2 (t1:c2)

rank' (Node r x c) = r

root E = Nothing
root (Node r x c) = Just x

insTree t [] = [t]
insTree t h@(t':ts)
    | rank' t < rank' t' = t : h
    | otherwise = insTree (link t t') ts

mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg h1@(t1:ts1) h2@(t2:ts2)
    | rank' t1 < rank' t2    = t1 : mrg ts1 h2
    | rank' t1 > rank' t2    = t2 : mrg h1 ts2
    | otherwise             = insTree (link t1 t2) (mrg ts1 ts2)

removeMinTree [] = Nothing
removeMinTree [t] = Just (t,[])
removeMinTree (t:ts)
    | r < r' = Just (t,ts)
    | otherwise         = Just (t',t:ts')
    where Just (t',ts') = removeMinTree ts
          Just r = root t
          Just r' = root t'

