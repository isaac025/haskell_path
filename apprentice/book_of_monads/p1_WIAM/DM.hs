module DM where

import Prelude hiding ((++))

data Tree a = Leaf a | Node (Tree a) (Tree a)

type WithCounter a = Int -> (a, Int)

type State s a = s -> (a,s)

numberOfLeaves :: Tree a -> Integer
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node l r) = numberOfLeaves l + numberOfLeaves r

relabel :: Tree a -> WithCounter (Tree (a, Int))
relabel (Leaf x) = \i -> (Leaf (x,i), i + 1)
relabel (Node l r) = relabel l `next` \l' ->
                     relabel r `next` \r' ->
                     pure' (Node l' r')


next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> let (r, i') = f i in g r i'

next' :: State s a -> (a -> State s b) -> State s b
f `next'` g = \i -> let (r, i') = f i in g r i'

pure' :: a -> WithCounter a
pure' x = \i -> (x,i)

-- Ex 1.1
pure'' :: a -> State s a
pure'' x = \i -> (x,i)

-- Ex 1.2
(++) :: [a] -> [a] -> [a]
[] ++ ls2 = ls2
ls1 ++ [] = ls1
(x:xs) ++ ls2 = x:(xs++ls2)

-- Ex 1.3
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : (map' f xs)

singleton :: a -> [a]
singleton x = [x]

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat xs

type Name = String
data Person = Person { name :: Name, age :: Int } deriving (Show)

validatePerson :: String -> Int -> Maybe Person
validatePerson name age = validateName name `then_` \name' ->
                          validateAge age `then_` \age' ->
                          Just (Person name age)


validateName :: String -> Maybe Name
validateName name = undefined

validateAge :: Int -> Maybe Int
validateAge age = undefined

then_ :: Maybe a -> (a -> Maybe b) -> Maybe b
then_  v g = case v of 
    Nothing -> Nothing
    Just v' -> g v'

flatten :: Maybe (Maybe a) -> Maybe a
--flatten (Just (Just x)) = Just x
--flatten _ = Nothing
flatten oo = then_ oo id

flatten' :: State s (State s a) -> State s a
flatten' ss = next' ss id

concatMap' :: [a] -> (a -> [b]) -> [b]
concatMap' xs f = concat' (map' f xs)


