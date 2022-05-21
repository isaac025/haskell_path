{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module FAL where

import Prelude hiding (Either(..), Right(..))

type Name = String

data Person = Person { name :: Name, age :: Int } deriving (Show)

(<||>) :: Maybe a -> Maybe a -> Maybe a
Just x <||> _ = Just x
Nothing <||> other = other

validateName :: String -> Maybe Name
validateName s = validateNameEnglish s
             <||> validateNameSpanish s
             <||> validateNameDutch s


validateNameEnglish :: String -> Maybe Name
validateNameEnglish s = undefined
validateNameSpanish :: String -> Maybe Name
validateNameSpanish s = undefined
validateNameDutch :: String -> Maybe Name
validateNameDutch s = undefined

data Either e r = Left e | Right r

-- Ex 7.1
instance Functor (Either e) where
    fmap _ (Left e) = Left e 
    fmap f (Right r) = Right (f r)

instance Applicative (Either e) where
    pure = Right
    Left e <*> _  = Left e
    Right f <*> r = fmap f r

instance (Semigroup a, Semigroup b) =>  Semigroup (Either a b) where
   Right x <> Right y = Right (x <> y)
   Right x <> _ = Right x
   _ <> Right x = Right x
   Left l <> Left l' = Left l

instance (Monoid a, Monoid b) => Monoid (Either a b) where
    mempty = Right mempty
    Left x `mappend` Left y = Left (x <> y)
    Left x `mappend` _ = Left x
    _ `mappend` Left y = Left y
    Right x `mappend` Right y = Right (x <> y)

instance Monad (Either e) where
    Left l >>= _ = Left l
    Right r >>= f = f r

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- Ex 7.2
instance (Monoid e) => Alternative (Either e) where
    empty = Left mempty
    (Right r) <|> _ = Right r
    _ <|> (Right r) = Right r
    (Left  l) <|> (Left  l') = Left l

instance Alternative [] where
    empty = []
    (<|>) = (++)

instance Alternative Maybe where
    empty = Nothing
    (Just r) <|> _ = Just r
    _ <|> (Just r) = Just r
    Nothing <|> Nothing = Nothing

guard :: Alternative m => Bool -> m ()
guard True = pure ()
guard False = empty

readMay :: String -> Maybe Int
readMay s = return (read s :: Int)

validateAge :: String -> Maybe Int
validateAge s = do n <- readMay s
                   guard (n >= 18)
                   return n

asum :: (Traversable t, Alternative m) => t (m a) -> m a
asum  = foldr (<|>) empty

msum :: (Traversable t, MonadPlus m) => t (m a) -> m a 
msum = foldr mplus mzero

mflilter :: (MonadPlus m) => (a -> Bool) -> m a -> m a
mflilter p x = do x' <- x
                  if p x' then return x' else mzero

people :: [Name]
people = ["Alejandro", "Elena", "Quique", "John", "John", "Mary", "Tom"]

pcRels :: [(Name,Name)]
pcRels = [ ("Alejandro", "Quique")
         , ("Elena", "Quique")
         , ("John", "Mary")
         , ("John", "Tom")
         , ("Mary", "Tim")
         , ("Quique", "Juan")
         ]

gpgcRels :: [(Name,Name)]
gpgcRels = do (grandp, parent) <- pcRels
              (parent', grandc) <- pcRels
              guard (parent == parent')
              return (grandp, grandc)

-- Ex 7.3
siblingRels :: [(Name, Name)]
siblingRels = do (parent, child) <- pcRels
                 (parent', child') <- pcRels
                 guard (parent == parent')
                 guard (child /= child')
                 return (child, child')

sums, ptys :: [Integer] -> [(Integer, Integer, Integer)]
{- with do
sums ns = do x <- ns
             y <- ns
             z <- ns
             guard (x + y == z)   
             return (x,y,z)
-}
sums ns = ns >>= \x -> ns >>= \y -> ns >>= \z ->
          guard (x + y == z) >> return (x,y,z)

ptys ns = do x <- ns
             y <- ns
             z <- ns
             guard (x*x + y*y == z*z)
             return (x,y,z)

triples ns = sums ns <|> ptys ns

-- Ex 7.4
--fairTriples :: [Integer] -> Logic (Integer, Integer, Integer) 
--
-- Ex 7.5
class Monad m => MonadError e m | m -> e where
    throwError :: e -> m a
    catchError :: m a -> (e -> m a) -> m a

instance MonadError e (Either e) where
    throwError = Left
    catchError (Left l) f = f l
    catchError (Right r) f = Right r
