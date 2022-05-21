{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module UFMC where

import Prelude hiding (mapM, sequence)
import Control.Monad hiding (mapM, sequence)

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x:xs) = do r <- f x
                   rs <- mapM f xs
                   return (r:rs)
{- or
 - mapM f (x:xs) = (:) <$> f x <*> mapM f xs
-}

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = do r <- x
                     rs <- sequence xs
                     return (r:rs)

{- or
 - sequence (x:xs) = (:) <$> x <*> sequence xs
-}

{- evolve mapM to
 - mapM f = sequence . map f
-}

-- Ex 4.1
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM _ [] _ = return []
zipWithM _ _ [] = return []
zipWithM f xs ys = sequence $ zipWith f xs ys
-- (:) <$> (f x y) <*> (zipWithM f xs ys)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n = sequence . replicate n

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
--filterM f [] = return []
filterM f = undefined -- sequence . filter f

statefulAction :: IO ()
statefulAction = void $ forM  ["Isaac", "Alejandro", "Mirna"]
                      $ \name -> print ("Hello, " ++ name)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM = liftM3 (\c t e -> if c then t else e)

newtype Identity a = I a deriving (Show)

instance Monad Identity where
    return :: a -> Identity a
    return x = I x -- return = id

    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (I x) >>= f = f x -- (>>=) = id

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (I x) = I (f x)

instance Applicative Identity where
    pure :: a -> Identity a
    pure x = I x

    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (I f) <*> (I x) = I (f x)
