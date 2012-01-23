> {-# LANGUAGE FlexibleInstances, OverlappingInstances, UndecidableInstances, NoMonomorphismRestriction #-}
> module P30 where

===============================================================================
Surprisingly there are only three numbers that can be written as the sum of fourth
 powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
===============================================================================


First of all, it's fairly reasonable to assume that there are not many numbers
equal to the sum of the fifth power of there digits.

Lets first try to count the unique sums of fifth powers, or really, unique sums
of nth powers.

We know that the largest number representable with a sum of fifth powers is the
arbitrary sum of 9^5. and the smallest representable such number, is 11, because
it must be a sum. Lets focus on coming up with some harder bounds on that range.

We'll start by looking for the lowest representable number, or LRN. 

We know that the LRN obeys this identity:

  f(k) = Sum(a_i * 10^i, 0 <= i <= n) = Sum(a_i^5, 0 <= i <= n) 
  
for some series a_1, a_2, ... a_n, where each a is on the range of integers
[0,9]. What we need here is a way to express "take the sum of the fifth powers
of the digits" in a way that does not involve talking about the digits
directly. Ideally, we want to have a completely algebraic way of saying this.

For now, it suffices to make use of our `digits` function from PELib

> import PELib.PELib
> import Data.Monoid
> import PELib.Convergence

to simply get a sense for where these numbers lie. Let's look, particularly, at
the difference of the sum-of-fifth powers and the number itself.

> sum_of_fifth_powers :: Integer -> Integer
> sum_of_fifth_powers = sum . map (^5) . digits 

the list of differences is just:

> differences = [x - (sum_of_fifth_powers x) | x <- [1..]]

You can see a few in the middle say, from 1000 to 2000:

> sample = drop 1000 . take 2000 $ differences


and not that we have a pretty bouncy list -- lots of positives and negatives
(indicating where numbers are much more than (or much less than) their
sum-of-fifths. 

So, we can then attempt to establish an upperbound. At some point, obviously,
the sum-of-fifths of `n` will _always_ be less than `n`. We can find this point
by noting that the largest s.o.f of `n` will be when `n` is a series of `9`s (cf
P34), So we solve

  10^(k+1) - 1 < 9^5 * k

for `k`, then `10^k+1 - 1` is our upper bound, fortunately, this is easy (as in
P34) to write a convergence algorithm for. In P34, we wrote a convergence algo
for our specific case, here, we'll write one for _any_ convergence.

This requires a bit of work (and the definition of things like typeclasses for
Group, Abelian Group, and a Dense Metric Space) so it's hidden off in
PELib.Convergence


Once we have all that work done (and beware, it's a bit finicky, sometimes it
will diverge wildly, just try flipping compare in that case). We can plug in:

> converge_value :: [Double]
> converge_value = convergence (\x -> compare (10**x) (9^5 * (x-1) + 1)) 5

which gives us a value of `5.416298112645895`. This constrains our search to
values less than `10**5.42` (give or take), or a little over 250,000. 

Let's just try for a brute force approach, to see if it works. It (notably)
doesn't for P34, but that is also about 4 times the search space.

> solve_thirty = sum [x | x <- [10..264027], sum_of_fifth_powers x == x]

Sure enough, that returns reasonably quickly, giving us an answer.
