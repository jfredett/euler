> module P25 where

================================================================================================
The Fibonacci sequence is defined by the recurrence relation:

    Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.

Hence the first 12 terms will be:

    F1 = 1
    F2 = 1
    F3 = 2
    F4 = 3
    F5 = 5
    F6 = 8
    F7 = 13
    F8 = 21
    F9 = 34
    F10 = 55
    F11 = 89
    F12 = 144

The 12th term, F12, is the first term to contain three digits.

What is the first term in the Fibonacci sequence to contain 1000 digits?
================================================================================================

Since we have arbitrarily sized integers, it would make sense to go for a direct approach, but really, this is a bad idea, we will rapidly find that the naive algorithm for fibonacci numbers dies. It's an O(2^n) algorithm, thats not good. We could use the following Identity?

Sum(F(i)^2, 0 <= i <= n) / F(n) = F(n+1)

We can keep the running sum in memory, and add on arbitrarily. It'll cost a few multiplies and a divide, and we can generate them lazily. Lets try it:

> fib :: [(Integer, Integer)]
> fib = fibGen 0 1 0

> --      running sum to f(n-1)  F(n)    curr n        list
> fibGen 	:: Integer -> Integer -> Integer -> [(Integer, Integer)]
> fibGen s f i	= (i,f) : (fibGen s' f' (i+1)) 
>		where 	f' = s' `div` f 
>			s' = (s + f^2) 

Now we can do a takeWhile until we meet the thousand digit requirement

> solvetwentyfive :: (Integer, Integer)
> solvetwentyfive = head $ dropWhile (\(_,x) -> (length $ show x) /= 1000) fib

