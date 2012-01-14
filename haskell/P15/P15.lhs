> module P15.P15 where

===============================================================================
Problem 15
19 April 2002

Starting in the top left corner of a 2x2 grid, there are 6 routes (without backtracking) to the bottom right corner.

How many routes are there through a 20x20 grid?
==============================================================================

Heres a simple recursive formula for solving the problem:

Heres the idea, 
We know that if our current x or y position is 20, then we can only go in the
other direction.

We also know that if we are at location 20,20; we have found a new path.

So the function is:

> --countPaths takes (x,y) * BOARDSIDESIZE and returns COUNTOFALLPATHS FROM
> --(x,y) to (BOARDSIDESIZE,BOARDSIDESIZE)
> countPaths :: (Integer,Integer) -> (Integer, Integer) -> Integer
> countPaths (x,y) (xmax,ymax)
>    | x == xmax                          = 1 --we can short circuit here and
>    | y == ymax                          = 1 --below, see Note 1
>    | otherwise                          = (countPathHelp (x+1,y)) + (countPathHelp(x,y+1))
>    where countPathHelp (x,y) = (countPaths (x,y) (xmax,ymax))

Note 1 
     We can short circuit with these, and it prevents us from needing to check
(x,y) = (s,s), because if we hit at any point (s,y) or (x,s), we know that we'll
just increment y or x (resp) until we hit (s,s). 

so solvefifteen is easy

> solvefifteen' :: Integer
> solvefifteen' = (countPaths (0,0) (20,20))  --19 because we index at 0.

=====================================


Notably, this is also solve by the math equation:

C(2n,n) where C(k,n) is k choose n

so solvefifteen is also

> fac :: Integer -> Integer
> fac n = product [1..n]

> solvefifteen :: Integer -> Integer
> solvefifteen n = (fac (2*n)) `div` ((fac n)^2) 
                      
