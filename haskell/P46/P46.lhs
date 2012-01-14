> module P46.P46 where    
> import qualified Data.Set as S

=============================================================================
Problem 46, Project Euler:
projecteuler.net

It was proposed by Christian Goldbach that every odd composite number can be
written as the sum of a prime and twice a square.

9 = 7 + 2x1²
15 = 7 + 2x2²
21 = 3 + 2x3²
25 = 7 + 2x3²
27 = 19 + 2x2²
33 = 31 + 2x1²

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime
and twice a square?
=============================================================================
 
First, lets note that the question is asking the following:

for some n,k,p where p is a prime >= 3 (we'll deal with the case p=2
 seperately); n,k > 0; n,k are integers; non are necessarily equal
the conjecture is equivalent to:

2k+1 = p + 2n² ==> 2k = p-1 + 2n²

now let q = p-1, which is even by definition

2k = 2q + 2n² ==> k = q + 2n²  

In the case p=2, we have:

2k+1 = 2 + 2n² ==> k = (2n² + 1)/2, 

so for all choices of n, k is not an integer, so p cannot be equal to 2.
(because n² retains the parity of the number, and 2n² forces it to be even, so
2n²+1 must be odd)

so, we want to filter a stream of numbers to generate falses if the number is of
the form:

k = q + 2n²,

noting that q is not necessarily prime.
==============================================================================

> import PELib.PELib

now lets create a stream of q's

> qlist :: [Integer]
> qlist = [(x-1) `div` 2) | x <- (drop 1 primes)]

what that does is apply the "to q" function across a list
of all but the first element. The list I use here is the list of primes, so we
end up with a list of q's

Now that we have a list of q's, we can create a list of 2n²'s, 

> nsquares :: [Integer]
> nsquares =  [2*x^2 | x<-[1..]]

Now what? We have two infinite lists, but how can we combine them effectively? I
think the best way here is to take a guess as to how deep we should go in
combining them, here's how we'll do that:

