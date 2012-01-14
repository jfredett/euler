> module P14.P14 where

===============================================================================
Problem 14
05 April 2002

The following iterative sequence is defined for the set of positive integers:

n n/2 (n is even)
n 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:
13 40 20 10 5 16 8 4 2 1

It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.


The Collatz Function
  3n+1 if n is odd
  n/2  if n is even

> collatz :: Integer -> Integer
> collatz n
>     | even n   = n `div` 2
>     | odd n    = 3*n + 1

The Collatz Sequence Length counter
   returns the length of the collatz chain for some number

> colseq :: Integer -> Integer
> colseq 1  = 1
> colseq n  = 1 + colseq(collatz(n))

The Maximum Sequence finder
  finds the maximum sequence length on a range

> colmax :: Integer -> Integer -> Integer
> colmax l h = maximum $ map colseq [l..h]


Now we just solve over 1 to 1000000

> solvefourteen :: Integer -> Integer -> (Integer, Integer)
> solvefourteen l h = rev $ maximum (zip (map colseq [l..h]) [l..h])

> rev :: (a,b) -> (b,a)
> rev (a,b) = (b,a)


 
     
        
