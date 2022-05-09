{-# LANGUAGE FlexibleContexts #-}

module BAWP where

size :: Num p => p
size = 2

pi' :: Floating a => a
pi' = 3.1459

radius :: Num p => p
radius = 10

circumference :: Floating a => a
circumference = 2 * pi' * radius

square :: Num p => p -> p
square x = x * x

sum_of_squares :: Num p => p -> p -> p
sum_of_squares x y = (square x) + (square y)

f :: Num p => p -> p
f a = sum_of_squares (a + 1) (a * 2)

abs' :: (Ord p, Num p) => p -> p
abs' x
    | x > 0     = x
    | x == 0    = 0
    | x < 0     = (-x)

sqrt' :: (Ord a, Fractional a, Num a) => a -> a
sqrt' x = sqrt_iter 1.0 x

sqrt_iter :: (Ord a, Fractional a, Num a) => a -> a -> a
sqrt_iter guess x = if (good_enough guess x) then guess 
                    else sqrt_iter (improve guess x) x

improve :: (Ord a, Fractional a, Num a) => a -> a -> a
improve guess x = average guess (x / guess) 

average :: (Ord a, Fractional a, Num a) => a -> a -> a
average x y = (x + y) / 2

good_enough :: (Ord a, Fractional a, Num a) => a -> a -> Bool
good_enough guess x = (abs' ((square guess) - x)) < 0.001

square' :: (Floating p, Num p) => p -> p
square' x = exp (double (log x))

double :: Num p => p -> p
double x = x + x

factorial :: (Ord p, Num p) => p -> p
factorial n = if n == 1 then 1
              else n * (factorial (n - 1))

factorial' :: (Ord p, Num p) => p -> p
factorial' n = fact_iter 1 1 n

fact_iter :: (Ord p, Num p) => p -> p -> p -> p
fact_iter product counter n = if counter > n then product
                              else fact_iter (counter * product) (counter + 1) n

fib :: (Ord p, Num p) => p -> p
fib n = fib_iter 1 0 n

fib_iter :: (Ord p, Num p) => p -> p -> p -> p
fib_iter a b count = if count == 0 then b
                     else fib_iter (a + b) a (count - 1)


count_change :: (Ord p, Num p) => p -> p
count_change amount = cc amount 5

cc :: (Ord p, Num p) => p -> p -> p
cc amount kinds_of_coins
    | amount == 0 = 1
    | (amount < 0) || (kinds_of_coins == 0) = 0
    | otherwise = (cc amount (kinds_of_coins-1)) + (cc (amount - (first_denomination kinds_of_coins)) kinds_of_coins)

first_denomination :: (Ord p, Num p) => p -> p
first_denomination kinds_of_coins
    | kinds_of_coins == 1 = 1
    | kinds_of_coins == 2 = 5
    | kinds_of_coins == 3 = 10
    | kinds_of_coins == 4 = 25
    | kinds_of_coins == 5 = 50

