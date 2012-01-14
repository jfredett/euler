> module P5.P5 where

================================================================================
Problem 5  "Least Common Multiple"
14 December 2001

2520 is the smallest number that can be divided by each of the numbers from 1 to
10 without any remainder.

What is the smallest number that is evenly divisible by all of the numbers from
1 to 20?
================================================================================

Easy, find the LCM of [1..20]

First, lets write lcm over a list, (haskell's prelude provides lcm over two
inputs)

> listLCM :: [Integer] -> Integer
> listLCM = (foldl lcm 1)

easy, now just do listLCM over a list from 1 to 20, hold on! make that 2 to
20... (lcm 1 x = x)

> solvefive :: Integer
> solvefive = listLCM [2..20]
