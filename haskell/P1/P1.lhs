> module P1.P1 where

===============================================================================
Problem 1 "Fizzbuzz"
05 October 2001

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get
3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
===============================================================================


First, lets find the time of solveone, we will generalize a little to say that
solveone should be a list of numbers containing all multiples of 3 or 5, so we
know then that solveone is an (infinite) list of integers:

> solveone :: [Integer]

Now lets decide how to implement that, we should first create a predicate which
returns true if the input is multiple of 3 or 5

> listpred :: Integer -> Bool
> listpred x
>    | x `mod` 3 == 0         = True
>    | x `mod` 5 == 0         = True
>    | otherwise              = False

Now we implement solveone w/ a simple list comprehension

> solveone = [n | n <- [1..], listpred n]

To solve, we simply need to filter out any element less than or equal to 1000,
and then take the sum of the resultant list, we know that the number will be
less than 1000, so we won't have to operate over the whole list.

> solution = foldr (+) 0 [ls | ls <- take 1000 solveone, ls < 1000]

