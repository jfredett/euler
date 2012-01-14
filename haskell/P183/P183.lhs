> module P183 where


===============================================================================
Problem 183
22 February 2008

Let N be a positive integer and let N be split into k equal parts, r = N/k, so 
that N = r + r + ... + r. 

Let P be the product of these parts, P = r × r × ... × r = rk.

For example, if 11 is split into five equal parts, 11 = 2.2 + 2.2 + 2.2 + 2.2 + 2.2, then P = 2.25 = 51.53632.

Let M(N) = Pmax for a given value of N.

It turns out that the maximum for N = 11 is found by splitting eleven into four equal 
parts which leads to Pmax = (11/4)4; that is, M(11) = 14641/256 = 57.19140625, 
which is a terminating decimal.

However, for N = 8 the maximum is achieved by splitting it into three equal parts, 
so M(8) = 512/27, which is a non-terminating decimal.

Let D(N) = N if M(N) is a non-terminating decimal and D(N) = -N if M(N) is a terminating decimal.

For example, ΣD(N) for 5 ≤ N ≤ 100 is 2438.

Find ΣD(N) for 5 ≤ N ≤ 10000.
===============================================================================

> import Data.Ratio


Really this is asking us to find the maximum of the function:

    
        k
       n
P(k) = -- 
        k 
       k

A little bit of calculus shows us the following:

      -k k 
P' = k  n  (1 + ln(k) - ln(n))

P' is the deriviative with respect to k

so, P' == 0 give us (with a little help from mathematica)

    n
k = - 
    e

where e is the base of the natural logarithm

showing this is a maximum is left to the reader

since we intend to keep it discrete, we'll have to check both the floor and ceiling of n/e

now that we know the maxima of P for any n, we need to find when said maxima is repeating/nonrepeating

We know that terminating decimals all have the form k/2^n5^m (from the wiki article on repeating decimals)
so we need a way to remove a factor of 2^n * 5^m from a given number N and see if anything remains. 

Heres a function which will remove any given factor from a number, 

-- the math is as follows --

x = r^k * (x/r^k) = r^k * p
ln(x) = ln(r^k * p) = ln(r^k) + ln(p) = k ln(r) + ln(p)

so 

ln(x) =~= ln(p) mod ln(r)

so then

e^(ln(x) mod ln(r)) = e^ln(p) = p which is what we want

 
> modF x y = 0 


> remFact r x = exp $ (log x) `modF` (log r)


> removeFactor :: Integer -> Integer -> Integer
> removeFactor r x 
>	| (x `mod` r) == 0 = removeFactor r (x `div` r)
>	| otherwise   	   = x


so heres the code we'll use

> isTerminating :: Integer -> Bool
> isTerminating x = x' == 1
>	-- we have this set up to remove 10's first, because it means that if x has form 2^k*5^j*p, with k<j (just for example)
>	-- we'll get after we're done x' = 2^(k-j)*p, and it will skip the middle step with minimal overhead. (two birds, one stone)
>	where x' = removeFactor 2
>		 $ removeFactor 5
>		 $ removeFactor 10 x

so our M(N) method looks like this

axN :: Integer -> Ratio Integer 

> maxN n = max n_lo n_hi
>	where k_lo = (floor $ (fromInteger n) / e) :: Integer 
>	      k_hi = k_lo + 1
>	      n_lo = p n k_lo
>	      n_hi = p n k_hi

> p :: Integer -> Integer -> Ratio Integer
> p n k = (n%k)^k

now we can write D(n) using the isTerminating bool

> d :: Integer -> Integer
> d n 
>  | isTerminating $ denominator n' = (-1)
>  | otherwise 			    = 1
>  where n' = maxN n

> e = exp 1


> solveoneeightythree = sum $ map d [5..10000]
