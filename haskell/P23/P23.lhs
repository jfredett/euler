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

Heres the plan, we take the range 24-28123, we then subtract and keep a list of
all possible abundants we can subtract from each element of the list, without
resulting in a negative integer. We then look to see if no element of the list
is abundant, if so, we add that to the sum, otherwise, we throw it away.


> import PELib.NumberClasses
> import PELib.PELib
> import Data.Set ( Set, fromList, fromDistinctAscList
>		  , empty, findMin, deleteMin, union, singleton
>		  , fold, (\\))
> import qualified Data.Set as S

Lets get our list of abundant numbers, we only need up to 28123, because we're
only testing that high.

> abundants :: Set Integer
> abundants = fromDistinctAscList [x | x <- [2,4..28123], isAbundant x] 

We can now do a cartestion product to get abundants^2, this will show us all possible sums of two
abundant numbers. After that, we can take the complement of the set against the set of all 
numbers from 1 to 28123, and get the set of all numbers not representable by the sum of two 
abundants. Fold in a sum, and were done.

> abundantrange :: Set Integer
> abundantrange = fromDistinctAscList [1..28123*2] -- 28123]

> abundants_squared = abundants >< abundants

> (><) 		:: (Ord a, Ord b) => Set a -> Set b -> Set (a,b)
> s >< t 	=  unions $ S.map (\s' -> S.map (\t' -> (s',t')) t) s
>		where unions = fold union empty

> abundantnonsums :: Set (Integer,Integer) -> Set Integer -> Set Integer
> abundantnonsums s t = t \\ s'
>		where s' = S.map (uncurry (+)) s

> solvetwentythree :: Integer
> solvetwentythree = setsum 
>		   $ abundantnonsums abundants_squared abundantrange 
>		   where setsum = fold (+)  0
