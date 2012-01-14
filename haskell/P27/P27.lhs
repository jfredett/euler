> module P27 where

===============================================================================
Euler published the remarkable quadratic formula:

n² + n + 41

It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. 
However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when 
n = 41, 41² + 41 + 41 is clearly divisible by 41.

Using computers, the incredible formula  n²+ 79n + 1601 was discovered, which produces 80 primes
for the consecutive values n = 0 to 79. The product of the coefficients, 79 and 1601, is 126479.

Considering quadratics of the form:

    n² + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n
    e.g. |11| = 11 and |-4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that produces the
maximum number of primes for consecutive values of n, starting with n = 0.
===============================================================================

We know that when n = b, f(n) = n^2 + an + b, f(b) will be divisible by b, so obviously it's 
not prime. Now lets consider the case n = 0, then we have f(n) = b, and we know this must be 
prime, consider f(1), f(1) = 1^1 + 1*a + b = 1 + a + b, which must also be prime. 

Now we have three conditions on the polynomial f(n), we can similarly derive an infinite number of conditions, by turning f into the following function

> import PELib.PELib
> primePolyCondition :: Integer -> Integer -> Integer -> Bool
> primePolyCondition n a b = isPrime $ n*n + a*n + b

What we now want to do is test every pair of (a,b) and find the pair that satisfys the longest 
series of consecutive primePolyConditions, starting with n=0

What other, more general conditions will help us to determine that a pair will pass more readily?

We might consider only trying coprime pairs. Lets look at f(n) with non-coprime pairs, 
if gcd(a,b) = d, d /= 0. 

n^2 + an + b =  
n^2 + a'dn + b'd =
n^2 + d(a'n + b') = 
n^2 + d(k) where k = (a'n + b') 

which means the test will fail when n = sqrt d, or at least when d|n because:

(sqrt d)^2 + d(k) = d + d(k) = d(1+k) which is not prime
or
d | n => n = dq for some q, so (dq)^2 + dk = d(dq^2 + k) which is not prime (because d is not 1)

in fact, f(n) will fail as soon as gcd(n,d) /= 1. Assume gcd(n,d) = m, m /= 1

f(n) = (mn')^2 + d'mk = m(mn'^2 + d'k) which is not prime, since m/=1  

So lets only consider pairs which are coprime, this will _drastically_ reduce the number of pair's we'll have to search through.

what other conditions should we look for? Well, the f(0) = b => b is prime condition is very 
important, no pair can even place without this, and even f(1) = 1 + a + b => 1+a+b is prime should probably be checked, it's worth doing to help weed out the trivial cases. We can note that
since primes cannot be negative, we can actually make the list b draws on fairly small. 

Also, we can see from the f(1) condition that a must be odd, since if it were not, than the sum would be even, and since b >= 2 (since b must be prime), then 1 + b is even in all cases except b = 2, so for 1 + a + b to be prime, |a| must be odd, but only if b is not 2.

So ideally, we want to prove b can never be 2, because it's a pain in the ass otherwise. Since
we have the condition that a and b are coprime, then a must be odd, or else gcd(a,b) would be 2, not 1, which contradicts our assumption that a and b are coprime. However, since this means that the f(1) condition would fail, since 1 + b is odd when b = 2, but since a is also odd, 1 + a + b = 3 + a => 3 + a is even, so 3 + a is not prime. Therefore, a must be odd.

In case you think that none of this is helping, I've been editing the function we're about to write as we've been going, and it's gone from 2000^2 cases, to ~600,000 cases, to about 38,000 casesnow. I think we can get it down even further.

is there anything else we can prove about a? (since we've already boiled down b fairly well?) ideally, we'ed like to prove that |a| is something nice, like always prime, or always positive, we also want to keep our conditions to only involving a and b. Since we can't test things about n 
during the generative phase.

An easy thing to deduce is that a can't equal b, or else gcd(a,b) /= 1

Obviously, our first few tests must be positive, which could possibly fail if a is negative and 
|a| is to big, so we can say that |a| < b, so that f(1) won't fail, (since if |a| > b, a + b => b - a => (b-a) < 0). In fact, we can say that |a| - b >= 2, or else f(1) would fail. (That brings our list of cases down to 13000 or so.)

Lets also test the |a| < b case with f(b-1), since we want it to succeed till this point,
(assume a is |a| for brevity)

(b-1)^2 + a(b-1) + b > 0 =>
b^2 - 2b + 1 + ab + a + b > 0 =>
b^2 - b + ab + a + 1 > 0, in fact, we can say it must be:
b^2 - b + ab + a + 1 >= 2 => b^2 - b + ab + a >= 1 => b(b - 1 + a) + a > 0. 

Since f(1) = 1 + a + b must be prime, then if a < 0, b+1 - a must be prime,

f(2) = 4 + 2a + b must be prime, 

Now let's generate the list of pairs to test

> possibleCandidates :: [(Integer,Integer)]
> possibleCandidates = [(a,b) | a <- [-999,-997..999], b <- [p | p <- [2..1000], isPrime p] 
>			      , a /= 0, b /= 0, a /= b, (b^2 - b + (abs a)*b + (abs a) >= 1)
>			      , b - (abs a) >= 2, gcd a b == 1
>			      -- just prefilter out those cases which fail quickly, so we don't
>			      -- run a full suite of tests on them.
>			      , primePolyCondition 1 a b, primePolyCondition 2 a b
>			      , primePolyCondition 3 a b, primePolyCondition 4 a b
>			      , primePolyCondition 5 a b]

Now the question is, how do we test for the best pair? Well, lets work by just testing the 
"quality" of one pair, and then we'll worry about the other stuff. 

We want to test starting at zero, and keep count of how many poly-conditions it passes.

We'll use a custom datatype to help with filtering later.

> data ScoredPair = SP 	{ pair  :: (Integer, Integer)
>			, score :: Int
>			}
>		deriving (Show, Eq)
> instance Ord ScoredPair where
> 	spA <= spB = (score spA) <= (score spB)


> testCandidate :: (Integer, Integer) -> ScoredPair
> testCandidate (x,y) = SP (x,y) score 
>		    -- since we've tested conditions f(0) and f(1) in the generation
>		    -- phase, we won't bother here.
>		where polyTestList = [primePolyCondition n x y | n <- [6..y]]
>		      score = length 
>			    -- since polyTestList is a list of bools, id :: Bool -> Bool here
>			    $ takeWhile (id) polyTestList  

now we'll just map the the testing function over the list of possible candidates, and take the minimum score, because of our Ord instance, thats just maximum, print and win!

> solvetwentyseven :: [ScoredPair]
> solvetwentyseven = map testCandidate possibleCandidates



NOTES:::

 Figure out while we're still getting so many cases with score == 0, like 337,11, etc.


