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

It is known that any multiple of an abundant number is itself abundant.

The key insight, we don't need every abundant number, merely all the 'least'
abundant numbers, that is, we need all the abundant numbers such that it is not
the of the form `2k`, where `k` is an abundant number. Once we have these
'minimally' abundant numbers, we can easily start folding in multiples of these,
and once we've folded in the multiples, it's a simple matter of finding the
closure under sum.

> abundants_less_than lim = abundants_helper [12..lim] lim []

> abundants_helper [] _ acc = acc
> abundants_helper (x:xs) lim acc 
>   | isAbundant x = abundants_helper next_xs lim (ms ++ acc)
>   | otherwise    = abundants_helper xs lim acc
>   where ms = multiples x lim
>         next_xs = filter (not . flip elem ms) xs


> multiples v limit = takeWhile (<=limit) [k * v | k <- [1..]]
                  

            
