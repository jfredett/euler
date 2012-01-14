> module P35 where

===============================================================================
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
===============================================================================

> import Data.List
> import PELib.PELib

> numberToList x   
>	| x >= 10   = lowDigit : numberToList highDigits
>	| otherwise = x : [] 
>	where (highDigits,lowDigit) = x `divMod` 10

> listToNumber x = sum $ zipWith (\x y -> x*(10^y)) x [0..]

> cycleNumber x = map listToNumber 
>		$ init 
>		$ zipWith (++) (tails ntlx) (inits ntlx)
>	where ntlx = numberToList x

> candidatePrimes = [p | p<-primes, (null $ (numberToList p) `intersect` [0,2,4,5,6,8])]

> cyclicPrime x = all isPrime $ cycleNumber x

> winnerPrimes = 2 : 3 : (filter cyclicPrime $ takeWhile (<=(10^6)) candidatePrimes)

> solvethirtyfive = length winnerPrimes

