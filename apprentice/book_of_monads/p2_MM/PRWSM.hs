module PRWSM where

--import Control.Monad.Reader

newtype State s a = State { runState :: s -> (a, s) } 

instance Functor (State s) where
    fmap f (State x) = State (\s -> let (a, s') = x s in (f a, s'))

instance Applicative (State s) where
    pure x = State (\s -> (x, s))
    (State f) <*> (State x) = State $ (\s -> let (fa,fs) = f s
                                                 (a, s') = x fs
                                             in (fa a, s'))

-- Ex 6.1
instance Monad (State s) where
    return x = pure x
    (State x) >>= f = State (\s -> let (a, s') = x s
                                       (State g) = f a
                                   in g s')

-- BAD CODE
nextValue :: State Int Int
nextValue = State (\i -> (i, i+1))
-- BAD CODE

get :: State s s
get = undefined

put :: s -> State s ()
put s = undefined

-- Ex 6.2
modify :: (s -> s) -> State s ()
modify f = do s <- get
              put (f s)
              return ()

-- GOOD CODE
nextValue' :: State Int Int
nextValue' = do i <- get
                put (i+1)
                return i
-- GOOD CODE

--runState :: State s a -> s -> (a,s)
evalState :: State s a -> s -> a
evalState c = fst . runState c

execState :: State s a -> s -> s
execState c = snd . runState c

------------------
-- START READER --
------------------
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader x) = Reader (f . x)

instance Applicative (Reader r) where
    pure x = Reader (\_ -> x)

    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return x = Reader (\_ -> x)
    
  x >>= f = Reader $ \env -> let x' = runReader x env 
                             in runReader (f x') env

ask :: Reader r r
ask  = Reader (\env -> env)

withReader :: (r -> s) -> Reader s a -> Reader r a
withReader m reader = Reader (\env -> runReader reader (m env))
----------------
-- END READER --
----------------

------------------
-- START WRITER --
------------------
newtype Writer w a = Writer { runWriter :: (w, a) }

instance Monoid w => Monad (Writer w) where
    return x = Writer (mempty, x)

instance Applicative (Writer w) where
    (Writer wab) <*> (Writer wa) = undefined


instance Functor (Writer w) where
    --fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer (w,x)) = Writer (w, f x)

tell :: w -> Writer w ()
tell w = Writer (w, ())
------------------
-- END WRITER --
------------------

newtype Predicate a = Predicate { runPredicate :: a -> Bool }

through :: (a -> b) -> Predicate b -> Predicate a
through f (Predicate p) = Predicate (p . f)

class Contravariant f where
    contramap :: (a -> b) -> f b -> f a

instance Contravariant Predicate where
    contramap = through

-- Ex 6.3
newtype Returns r a = Returns { runReturns :: a -> r }

instance Contravariant (Returns r) where
    contramap f (Returns r) = Returns (r . f) 
