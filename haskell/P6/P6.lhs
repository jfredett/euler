> module P6.P6 where 

===============================================================================
The sum of the squares of the first ten natural numbers is,
1² + 2² + ... + 10² = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)² = 55² = 3025

Hence the difference between the sum of the squares of the first ten natural
numbers and the square of the sum is 3025 385 = 2640.

Find the difference between the sum of the squares of the first one hundred
natural numbers and the square of the sum.
===============================================================================

Also, with Haskell's list comprehensions, totally easy.

First, lets define a function which takes the sum.

> listSum :: [Integer] -> Integer
> listSum = (foldl (+) 0)

Easy, now lets define our two sums we need

> sumSquares :: Integer -> Integer -> Integer
> squareSum :: Integer -> Integer -> Integer

> sumSquares lo hi = listSum [n*n | n <-[lo..hi]]
> squareSum lo hi = (listSum [lo..hi])^2

Now we just define the solver function:

> solvesix :: Integer -> Integer -> Integer
> solvesix lo hi = (squareSum lo hi) - (sumSquares lo hi)

While were here, we can actually solve this pretty quickly in general,

We know that the sum of a series of Squares is:

n(n+1)(2n+1)
-----------
     6

We know that the sum of some series starting at m (and incrementing by 1 each
time), squared, is:

              2             2
((n+m)(n-m+1))      (n(n+1))
(------------)   == -------- 
(      2     )         4

The difference of these is:

  4     3     2
3n  + 2n  - 3n  - 2n
--------------------
         12

we then simply evaluate at 100

> -- note, this only evaluates from 1, not from any starting point
> closedformsolvesix :: Integer -> Integer
> closedformsolvesix n = (3*n^4 + 2*n^3 - 3*n^2 - 2*n) `div` 12


