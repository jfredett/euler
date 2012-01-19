> module P97.P97 where

================================================================================
Problem 97
10 June 2005

The first known prime found to exceed one million digits was discovered in 1999,
and is a Mersenne prime of the form 2^6972593; it contains exactly 2,098,960
digits.  Subsequently other Mersenne primes, of the form `2^p - 1`, have been
found which contain more digits.

However, in 2004 there was found a massive non-Mersenne prime which contains
2,357,207 digits: 28433 * 2^7830457 + 1

Find the last ten digits of this prime number.
================================================================================

> import PELib.PELib

more modular exp.

 solve_ninety_seven 

> solve_ninety_seven = (28433 * power + 1) `mod` (10^10)
>   where power = modExp 2 7830457 (10^10)
