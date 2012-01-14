> module P16.P16 where

===============================================================================
Problem 16

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?
===============================================================================

Easy, just do a function which computes the digital sum, fortunately, its in
PELib for us. :)

 import PELib.PELib

--for now, we'll just define it locally
--TODO: move this to PELib

> -- returns the reverse of the list of digits... :/
> toListOfDigits :: Integer -> [Integer]
> toListOfDigits k
>    | k < 10            = [k] 
>    | otherwise         = m : toListOfDigits d  
>    where (d,m) = k `divMod` 10

> digitalSum :: Integer -> Integer
> digitalSum k = sum (toListOfDigits k)


> solvesixteen :: Integer
> solvesixteen = digitalSum (2^1000)
