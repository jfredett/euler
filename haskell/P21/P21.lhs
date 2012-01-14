> module P21.P21 where
> import PELib.PELib 
> import Control.Monad.Fix

===============================================================================
Problem 21
05 July 2002

Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).
If d(a) = b and d(b) = a, where a b, then a and b are an amicable pair and each
of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
===============================================================================

First, we'll need a "divisorsOf" function

> divisorsOf :: Integer -> [Integer]
> divisorsOf k 
>    | k == 1       = []
>    | isPrime k    = [1]
>    | otherwise    = firstDivisor : leftover : (divisorsOf leftover)
>    where
>         firstDivisor = head $ dropWhile (\p -> not (p `divides` k)) primes
>         leftover = (k `div` firstDivisor)

This however is not the full divisor set. We need a form of closure on the set, 
really- we're looking for the generators of the Units group (well, ring) on the
number. The closure we need is

given a,b elements of D, where D is the set returns by "divisorsOf"
if a*b < k, then D = D union k
repeat until we've tried every a*b

This will work, mostly. but its far more effective to do the whole process this
way

for each x in [1..upperlimit]
  D = D union (gcd x k)

but whats the upperlimit? well, it the size of the greatest factor. We can find
that easily, using the same code as before.

Here's the new and improved divisorsOf

> divisorsOf' :: Integer -> [Integer]
> divisorsOf' k 
>    | k == 1       = []
>    | isPrime k    = [1]   
>    | otherwise    = removeDupes (firstDivisor : (map (gcd k) [1..gcdMax]))
>    where
>         firstDivisor = head $ dropWhile (\p -> not (p `divides` k)) primes
>         gcdMax = k `div` firstDivisor

This has the wonderful propery of getting every factor on the first run through,
but its slower, much much slower. So lets think- the first, faster function is
fast but not complete, and the latter is complete but not fast.

Let realize, though- that these divisors form a closed set under multiplication
'gcd' k, that is

gcd works sortof like mod does, but it dynamically forces different factors
down.

Psuedo Haskell:

D = [d | d `divides` k]
a,b elements of D => a*b `gcd` k `elem` of D

you can verify this on your own, but a quick example will show that in the
important case, that is a*b > k, like 5*110 for k = 220, is an element of the
set D, (notably it is equal to 110) 


> applyClosure :: Ord a => [a] -> (a -> a -> a) -> [a] 
> --applyClosure maps each element of a list under the binary function given
> -- and applies it to every other element of the list
> --we need to iterate this and take a 
> applyClosure ls f = (iterate (\l -> removeDupes $ applyClosure' l f) ls) !! 10
>                   where applyClosure' [] _ = []
>                         applyClosure' ls@(x:xs) f = (map (f x) ls) ++ (applyClosure' xs f)

so the final "divisors" function is the following

> improperDivisors :: Integer -> [Integer]                                                     
> improperDivisors k = applyClosure (divisorsOf k) (\ a b -> (a * b) `gcd` k)

notably, these are the improper divisors (they include k) since we know they are
sorted, we can rewrite it to just take the first bit.

> properDivisors :: Integer -> [Integer]
> properDivisors = (init . improperDivisors)

now lets create a predicate to determine whether a number is a member of an
amicable pair

> (#) :: (a -> a) -> Integer -> (a -> a)
> (#) f n
>    | n == 0  = id
>    | n == 1  = f
>    | otherwise = (f . (f#(n-1)))


> isAmicable :: Integer -> Bool
> isAmicable k = (not $ isPrime k)
>              && (not $ isPerfect k)
>              && (k == ((sum . properDivisors)#2) k) 
              
we'll notice that our isAmicable Predicate will (without that last bit) detect
perfect numbers too

> isPerfect :: Integer -> Bool
> isPerfect k = (not $ isPrime k) && (k == (sum . properDivisors) k)


so using the above we can write solvetwentyone

> solvetwentyone :: Integer
> solvetwentyone = sum $ [x | x<-[2..10000], isAmicable x]
