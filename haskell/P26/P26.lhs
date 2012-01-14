> module P26 where

===============================================================================================
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

    1/2	= 	0.5
    1/3	= 	0.(3)
    1/4	= 	0.25
    1/5	= 	0.2
    1/6	= 	0.1(6)
    1/7	= 	0.(142857)
    1/8	= 	0.125
    1/9	= 	0.(1)
    1/10=	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
================================================================================================

> import PELib.PELib
> import Data.List
> import Data.Function

From the wikipedia entry on "repeating decimals", we find that the "period of 1/p, p a prime, is equal to the order of 10 mod p"

we can write a order-finder simply


> order n k  
>	| (gcd n k) == 1 	= order' n n k
>	| otherwise		= 0
>	where order' o n k
>		| n == 1	= 1
>		| otherwise	= 1 + (order' o ((n * o) `mod` k) k)

so now we want to find the prime with the largest order of 10. 

> ordersOf n = map (\x -> (x, order n x)) primes
> ordersOf10 = ordersOf 10

so, under the assumption that the longest repeating decimal will be have a prime denominator, we just have to pick the one with the greatest order of 10 that is less than 1000

> solvetwentysix = maximumBy (compare `on` snd) $ takeWhile (\x -> (fst x) <= 1000) ordersOf10
