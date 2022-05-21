module IML where

-- Ex 5.1
-- Describe the two monoidal structures of Maybe

-- Ex 5.2
instance Semigroup Int where
    x <> y = x + y

instance Monoid Int where
    mempty = 0
    mappend = (+)
-- let 1 be mempty for Monoid of Mul
-- let 0 be mempty for Monoid of Add
-- exp mempty :: Add === mempty Mul
-- exp (x `mappend :: Add` y) === (exp x) `mappend :: Mul` (exp y)

