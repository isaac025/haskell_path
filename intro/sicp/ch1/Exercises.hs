module Exercises where

import BAWP
-- E 1.1
-- 10 returns 10
-- (+ 5 3 4) returns 12
-- (- 9 1) returns 8
-- (/ 6 2) returns 3.0
-- (+ (* 2 4) (- 4 6)) returns 6
-- (define a 3) returns nothing
-- (define b (+ a 1)) returns nothing
-- (+ a b (* a b)) returns 19
-- (if (and (> b a) (< b (* a b)))
--     b
--     a) returns 4
-- (cond ((= a 4) 6)
--       ((= b 4) (+ 6 7 a))
--       (else 25)) returns 16
-- (+ 2 (if (> b a) b a)) returns 16
-- (* (cond ((> a b) a)
--          ((< a b) b)
--          (else -1))
--    (+ a 1))a returns 16
--
-- E 1.2
-- (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* (* 3 (- 6 2))(- 2 7)))
--
-- E 1.3 
sum_of_2_largest_squares :: (Ord p, Num p) => p -> p -> p -> p
sum_of_2_largest_squares x y z
    | (x == y) && (y == z)  = sum_of_squares x y
    | (x > z) && (y > z)    = sum_of_squares x y
    | (x > z) && (z > y)    = sum_of_squares x z
    | (y > x) && (z > x)    = sum_of_squares y z

-- E 1.4
-- If b is positive then return a + b
-- otherwise a - b, since b is negative
-- a - (-b) === a + b
a_plus_abs_b :: (Ord n, Num n) => n -> n -> n
a_plus_abs_b a b = if b > 0 then a+b
                   else a-b

-- E 1.5
p :: (Ord a, Num a) => a -> a
p x = x 

test :: (Ord a, Num a) => a -> a -> a
test x y = if x == 0 then 0 else y

-- E 1.6
-- At least for me nothing!
new_if :: Bool -> a -> a -> a
new_if predicate then_clause else_clause
    | predicate = then_clause
    | otherwise = else_clause

sqrt_iter' :: (Ord a, Fractional a, Num a) => a -> a -> a
sqrt_iter' guess x = new_if (good_enough guess x) guess (sqrt_iter' (improve guess x) x)

sqrt'' :: (Ord a, Fractional a, Num a) => a -> a 
sqrt'' x = sqrt_iter' 1.0 x

-- E 1.7
-- E 1.8
cube_root :: (Ord a, Fractional a, Num a) => a -> a 
cube_root x = cube_root_iter 1.0 x

cube_root_iter :: (Ord a, Fractional a, Num a) => a -> a -> a
cube_root_iter guess x = if (good_enough' guess x) then guess
                         else (cube_root_iter (improve' guess x) x)

improve' :: (Ord a, Fractional a, Num a) => a -> a -> a
improve' guess x = ((x / (square guess)) + 2 * guess) / 3 

good_enough' :: (Ord a, Fractional a, Num a) => a -> a -> Bool
good_enough' guess x = (abs' (guess^3 - x)) < 0.001

-- E 1.9
-- E 1.10
ackermann :: (Ord a, Num a) => a -> a -> a
ackermann  x y
    | y == 0    = 0
    | x == 0    = 2 * y
    | y == 1    = 2
    | otherwise = ackermann (x - 1) (ackermann x (y - 1))

two_times_n :: (Ord a, Num a) => a -> a
two_times_n n = ackermann 0 n

two_exp_n :: (Ord a, Num a) => a -> a
two_exp_n n = ackermann 1 n

ackermann_2_nsquaredn :: (Ord a, Num a) => a -> a
ackermann_2_nsquaredn n = ackermann 2 n

five_times_nsquared :: (Ord a, Num a) => a -> a
five_times_nsquared n = 5 * n * n

-- E 1.11
f_recur :: (Ord p, Num p) => p -> p
f_recur n
    | n < 3 = n
    | otherwise = (f_recur (n - 1)) + (2 * (f_recur (n - 2))) + (3 * (f_recur (n - 3)))

f_iter:: (Ord p, Num p) => p -> p
f_iter n = f_iter_helper 1 n

f_iter_helper :: (Ord p, Num p) => p -> p -> p
f_iter_helper count n
    | count == n    = n
    | otherwise     = f_iter_helper (count + 1) ((n - 1) + (2*(n-2)) + (3*(n-3)))

-- E 1.12
-- E 1.13


