> module P7.P7 where

===============================================================================
Problem 7
28 December 2001

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that
the 6th prime is 13.

What is the 10001st prime number?
===============================================================================

Easy, import the seive from P46, then its a simple drop/take

> import qualified P46
> primes :: [Integer]
> primes = P46.plist
 
> solveseven :: [Integer]
> solveseven = take 1 (drop 10000 primes)
