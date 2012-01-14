> module P41 where

===============================================================================
We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

What is the largest n-digit pandigital prime that exists?
===============================================================================

This should be easy

> import PELib.PELib (isPrime)
> import Data.List (sort, delete)

> permutations [] = [[]]
> permutations ls  = [x:p | x <- ls, p <- permutations (delete x ls)]


> pandigitalsOfLength n = permutations [1..n]

> pandigitalListsOfLength n k = concatMap (\x -> pandigitalsOfLength x) [n..k]

> listToInteger ls = sum $ zipWith (\x y -> x*(10^y)) ls [0..]  

we can optimize the above writing a special case version

First, we can filter out anything not ending in [1,3,7,9] because we know they can't be prime

Second, we can ignore the following, using divisibility rules:

length 1 case, 1 is the only pandigital, and it's not prime
length 2 case, [12,21] are the only pandigitals, and they aren't prime
length 3 case, using the divisibility rules, 1+2+3 is divisible by 3, so any number with those digits is divisible by three.
	so non are prime
length 5 case, using a similar method to the above
length 6 case, similar to length 3 case
length 8 and length 9 case, similar to length 3

And finally, we can remove the length 4 with the following arguement
	first we know cases of the form "x2y4" and "y4x2" are all non-prime, we we are left with cases

> leftoverLength4cases = [4123,4321,2341,2143]
	
	we can test these directly, and use the big test on only the length 7 variant, however, it is highly likely that there
	is a pandigital prime of length 7, so we won't bother, if we come up with nothing, then we know that we were wrong, and 
	can come back to this.  

> solvefortyone = maximum $ [ listToInteger p | p <- pandigitalsOfLength 7 
>			    , head p `elem` [1,3,7,9], (isPrime . listToInteger) p ]



