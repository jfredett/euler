> module P20.P20 where

===============================================================================
Problem 20
21 June 2002

n! means n (n 1) ... 3 2 1

Find the sum of the digits in the number 100!
===============================================================================

With Haskell, it's easy

> fac n = product [1..n]
> bigFac = fac 100


---TODO:
--- find other digital sum implementation and move it to PELib

> digitalSum :: Integer -> Integer
> digitalSum k 
>    | k < 10       = k
>    | otherwise    = m + digitalSum d
>    where (d,m) = k `divMod` 10

> solvetwenty = digitalSum bigFac
