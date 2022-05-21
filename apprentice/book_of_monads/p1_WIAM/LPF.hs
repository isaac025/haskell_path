module LPF where

import Prelude hiding ((**))

-- Ex 3.1
plus :: Maybe Int -> Maybe Int -> Maybe Int
plus x y = do a <- x
              b <- y
              return (a+b)

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f x y = do a <- x
                 b <- y
                 return (f a b)


ap :: Monad m => m (b -> c) -> m b -> m c
ap f x = do f' <- f
            b <- x
            return (f' b)

-- Ex 3.2
fmap' :: (Applicative f, Functor f) => (a -> b) -> f a -> f b
fmap' f x = ((pure f) <*> x)

newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

instance Functor ZipList where
    fmap f (ZipList []) = ZipList []
    fmap f (ZipList (x:xs)) = ZipList ((f x):(fmap f xs))
    

instance Applicative ZipList where
   pure x = ZipList (repeat x)
   ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- E 3.4
triple :: (a,b,c) -> (a, (b, (c)))
triple (x,y,z) = (x, (y, (z))) 

tuples_4 :: (a,b,c,d) -> (a, (b, (c, (d))))
tuples_4 (x,y,z,w) = (x, (y, (z, (w))))

-- Ex. 3.5
--class Functor f => Monoidal f where
--  unit :: f ()
--  (**) :: f a -> f b -> f (a, b)

unit :: Applicative f => f ()
unit = pure ()

(**) :: Applicative f => f a -> f b -> f (a, b)
x ** y = (,) <$> x <*> y  

