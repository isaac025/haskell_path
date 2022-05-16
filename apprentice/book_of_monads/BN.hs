module BN where

import DM (Tree(..), Person(..), State(..), validateName, validateAge)

relabel :: Monad m => Tree a1 -> m (Tree a2)
--relabel (Leaf x) = \i -> (Leaf (i,x), i+1)
relabel (Node l r) = do
    l' <- relabel l
    r' <- relabel r
    return (Node l' r')
    
validatePerson name age = do
    name' <- validateName name
    age' <- validateAge age
    return (Person name' age')

validatePerson' name age = validateAge age >>= \age' ->
                           let uAge = age < 18
                           in validateName name >>= \name' ->
                              return (Person name' age')

put :: s -> State s ()
put s = undefined

get :: State s s
get = undefined 

incrCounter :: State Int Int
incrCounter = do n <- get 
                 --p <- put (n+1)
                 put (n+1)
                 return (n+1)
