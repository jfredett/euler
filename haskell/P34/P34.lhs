> module P34.P34 where

Problem 34
===============================================================================
January 2003

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their
digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included. 
===============================================================================


> digits :: Integer -> [Integer]
> digits = reverse . digits' 
>   where digits' x 
>           | x < 10    = [x]
>           | otherwise = (x `mod` 10) : digits' (x `div` 10)

> factorial 0 = 1
> factorial 1 = 1
> factorial x = product [2..x]

By analysis, we can show that -- unless the number is less than 7 digits, it
cannot be the sum of it's digits' factorials.

Given this, we can simply perform the following function on about 10 million
numbers, a la

> isFactorialSum :: Integer -> Bool
> isFactorialSum x = (sum . map factorial . digits $ x) == x

> solve_thirty_four_slow = sum [x | x <- [1..10^9], isFactorialSum x]

However, this is very slow. We should be able to do this more effectively.

Firstly, we can solve to a better resolution the upper bound. 

The upper bound is calculated based on the following principle. At some point,
the size of the number being calculated will be bigger than the possible sum of
it's factorial digits (which I will call "figits"). This is governed by the
equation:

        9! * k + 1 < 10^k 
    =>  362880k + 1 < 10^k

We can write a simple convergence solver for this, as follows:

> convergence old = convergence' old old 1

> convergence' old new adj 
>   -- old was too big and this was too big, don't change adj
>   | guess old && guess new         = new : convergence' new (new + adj) adj
>   -- turning point, decrease adj
>   |  guess old && not (guess new)  
>   || not (guess old) && guess new  = new : convergence' new new (adj / 2)
>   -- both small, subtract adj, don't adjust
>   | not (guess old || guess new)   = new : convergence' new (new - adj) adj
>   where guess x = (factorial . fromInteger $ 9) * x + 1 > 10**(x+1)

This converges to a value of `5.2851875`, That's the value of `k`, we are
concerned with `k+1`, so we need to search to `6.2851875`, which shaves off
about 8 million values. We can try this again with the new upper bound.

Note that 10**6.2851875 ~ 8071643 (it's a bit lower than that)

> solve_thirty_four_less_slow = sum [x | x <- [145..8071643], isFactorialSum x]

This, however, still takes too long. We can continue to analyze away some more
cases.

Notice -- every factorial is even except for 0 and 1. So, any odd number which
is the sum of it's figits will necessarily contain a 0 or a 1. So we can
exclude numbers based on this predicate

--> isFigitable x = even x || (0 `elem` (digits x) || 1 `elem` (digits x)) 

that'll get us down to ~7 million values. We can also make a stronger statement
-- it is only figitiable if it contains an odd number of 0's and 1's

> isFigitable x = even x 
>              || (odd.length $ [d | d <- digits x, d `elem` [0,1]])

This gets us to just under 6 million.

Next, consider



