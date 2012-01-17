> module P23.P23 where

===============================================================================
Problem 23
02 August 2002

A perfect number is a number for which the sum of its proper divisors is exactly
equal to the number. For example, the sum of the proper divisors of 28 would be
1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number whose proper divisors are less than the number is called deficient and
a number whose proper divisors exceed the number is called abundant.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
===============================================================================

> import PELib.NumberClasses
> import PELib.PELib
> import Data.Set ( Set, fromList, fromDistinctAscList
>		  , empty, findMin, deleteMin, union, singleton
>		  , fold, (\\))
> import qualified Data.Set as S

Lets start with a simpler case -- finding whether or not a given number is the
sum of two abundants. Lets call such numbers "affluent" (since it has two
abundant "children")

/*> isAffluent :: Integer -> Bool*/

We know that some abundant numbers can be affluent, since `24` is abundant, and
the sum of two abundants (`12` and `12`). So here's a natural question -- is
twice an abundant number always abundant? We can do a quick scan for
counterexamples with:

> counterexample_check = [ x | x <- [1..28123], isAbundant x && (not . isAbundant)(x * 2) ]

Since we only care that this is true for numbers less than 28123, we can just do
an exhaustive search like the above
