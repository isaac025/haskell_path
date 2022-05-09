module Exercises where

import DataStructures

-- Exercise 3.3
fromList :: Ord a => [a] -> LeftistHeap a
fromList xs = foldl (\acc x -> merge acc x) ELH xs'
    where xs' = map (\x -> insert x ELH) xs

fromList' :: Ord a => [a] -> WeightBiasedLeftistHeap a
fromList' xs = foldl (\acc x -> merge acc x) EWBLH xs'
    where xs' = map (\x -> insert x EWBLH) xs

fromList'' xs = foldl (\acc x -> merge acc x) (BH (0, [])) xs'
    where xs' = [(BH (0, [Node x []])) | x <- xs]

-- Exercise 3.6
-- implement binomial heap but 
-- use rank at the top level with the
-- tree.
data Tree a = Node a [Tree a] deriving (Show, Eq, Read)

newtype BinomialHeap a = BH (Int, [Tree a]) deriving (Show, Eq, Read)

instance Heap BinomialHeap where
    empty = BH (0, [])
    isEmpty (BH (_, ts)) = null ts

    insert x (BH (r, ts)) = BH (insTree (0, Node x []) (r,ts))
    merge (BH (r1,ts1)) (BH (r2,ts2)) = BH ((r1+r2), mrg (r1,ts1) (r2, ts2))

    findMin = undefined
    deleteMin = undefined

link (r, t1@(Node x1 c1)) (_, t2@(Node x2 c2))
    | x1 <= x2 = ((r+1), Node  x1 (t2:c1))
    | otherwise = ((r+1), Node x2 (t1:c2))

root (Node x c) = Just x

insTree (r1,t) (r2,[]) = ((r1+r2), [t])
insTree (r1, t) (r2, h@(t':ts))
    | r1 < r2   = (r1, t : h)
    | otherwise = insTree (link (r1, t) (r2, t')) (r2,ts) 

mrg (r1, ts1) (0, []) = ts1
mrg (0, []) (r, ts2) = ts2
mrg (r1, h1@(t1:ts1)) (r2,h2@(t2:ts2))
    | r1 < r2   = t1 : mrg (r1, ts1) (r2, h2)
    | r1 > r2   = t2 : mrg (r1, h1) (r2, ts2)
    | otherwise = newTree 
    where r' = r1 + r2
          ts' = mrg (r1, ts1) (r2, ts2)
          (r'', newTree) = insTree (link (r1, t1) (r2, t2)) (r', ts')

-- Exercise 3.9
fromOrdList :: Ord a => [a] -> RedBlackTree a
fromOrdList (x:xs) = foldl (\acc x -> insert' x acc) rbt xs
    where rbt = insert' x ERB
