> module P3.P3 where

================================================================================
Problem 3 -- "Factors"
02 November 2001

The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 317584931803?
================================================================================

Factoring! Ahh! 

Let's grab the primes list from the PE library to help us:

> import qualified PELib as PEL
> primes :: [Integer]
> primes = PEL.plist

Let's define that big number as a variable...

> bigNumber :: Integer
> bigNumber = 317584931803

Good, now we have our list of primes, and our big number defined convienently,
lets set up a factorize function, it will have type:

> factorize :: Int -> Integer -> [Integer]

That is, Factorize will return a list of factors, the second input integer will
give us the number of primes to test with.

This is relatively trivial to write, but we need a quick "divisible" predicate
first

> divisible :: Integer -> Integer -> Bool
> divisible x y = snd rs == 0 && y * fst rs == x where rs = x `divMod` y

With that, we can easily write factorize

> factorize t x = [p | p <- take t primes, divisible x p]

now lets do the factorization of the big number, I know its sqrt is 56546, so
we'll take about 5000, that should get us there.

> solvethree = factorize 5000 bigNumber


