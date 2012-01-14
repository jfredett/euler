> module P10.P10 where

===============================================================================
Problem 10
08 February 2002

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below one million.
===============================================================================

Easy easy easy,

> import PELib.PELib

Just walk until we hit something to big.

> solve :: [Integer] -> Integer
> solve ls = sum ls

> solveten :: Integer
> solveten = (solve (takeWhile (\a -> a <= 10^6) primes))

