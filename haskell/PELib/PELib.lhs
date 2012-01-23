> module PELib.PELib where
> import qualified Data.Set as Set
> import qualified PELib.Primes as Primes
> import Control.Monad.Fix
> import Control.Arrow

===============================================================================
Project Euler Library

Just some useful stuff for solving PE problems
===============================================================================
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\
\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\/
//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\
\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\//\\/
===============================================================================
fromInteger alias

> fI = fromInteger
> isPrime = Primes.isPrime
> nextPrime k 
>    | isPrime k    = Primes.nextPrime (k+1)
>    | otherwise    = Primes.nextPrime k

> isIntegral :: (RealFrac a, Floating a) => a -> Bool
> isIntegral d = (d - (fromInteger (floor d))) == 0

> forceToInteger :: (RealFrac a, Floating a) => a -> Integer
> forceToInteger d = ((floor d) :: Integer)

> digits :: Integer -> [Integer]
> digits = reverse . digits' 
>   where digits' x 
>           | x < 10    = [x]
>           | otherwise = (x `mod` 10) : digits' (x `div` 10)

===============================================================================
List of Primes
     Produces a list of the prime numbers, we use the HFM isPrime function for this

> primes :: [Integer] -- Infinite
> primes = 2:[k | k <- [3,5..], isPrime k]

Old Seive based version, slow.  :(
 primes = 2 : (seive [3,5..]) where seive (x:xs) = x : seive [y | y <- xs, y `mod` x /= 0]
                                                                                
===============================================================================
List of Composites
     Produces a list of mostly composite integers, depends on the implemenation of
primes.

> composites :: [Integer] -- Infinite
> composites = filter (not . isPrime) [1..]

===============================================================================
Remove Duplicate
     remove dupe entrys in a list, using the set library, kind of a hack

> removeDupes :: Ord a => [a] -> [a] 
> removeDupes = (Set.toList . Set.fromList)

Cartesian Product for Data.Set

> (><)          :: (Ord a, Ord b) => Set.Set a -> Set.Set b -> Set.Set (a,b)
> s >< t        =  unions $ Set.map (\s' -> Set.map (\t' -> (s',t')) t) s
>               where unions = Set.fold Set.union Set.empty

> crossWith :: (Ord a, Ord b, Ord c) 
>	    => (a -> b -> c) -> Set.Set a -> Set.Set b -> Set.Set c
> crossWith f s t = unions $ Set.map (\s' -> Set.map (\t' -> f s' t') t) s
>		where unions = Set.fold Set.union Set.empty

===============================================================================

isPerfPower
     An effectively linear algorithm for determining whether a number is a
perfect power or not, borrowed from Choko on #haskell, he actually implemented
isNotPerfPower. Modified to use integralLog instead of calculating from scratch

> isPerfPower :: Integer -> Bool
> isPerfPower x = not(isNotPerfPower x)

> isNotPerfPower n = 
>     null [b | b <- [2..ceiling(log(fromIntegral(n))/log(2))], 
>                    let a = exp(log(fromIntegral(n))/fromIntegral(b)) 
>                    in (((floor(a)^b) == n) || ((ceiling(a)^b) == n))]

> integralSqrt :: Integer -> Maybe Integer
> -- the algorithm below doesn't cover this special case
> integralSqrt 1 = Just 1
> integralSqrt n = integralSqrt' n (n `div` 2) 0
>       where integralSqrt' n' k t
>               | k^2 == n                = Just k
>               | (t > limit)             = Nothing
>               | otherwise               = if (k^2 < n)
>                                         then integralSqrt' k (succ k)  (succ t)
>                                         else integralSqrt' k (avg (-k) n') (succ t)
>             avg x y = (x + y) `div` 2
>             limit = n 



integralLog and isIntegralLog determines if the logarithm base b of k is integral,
 it uses the following fact:
                                                                            
(b ^ (floor (logBase b k)) == k) ==> Integral (logBase b k)

determines whether a logarithm is integral.

 isIntegralLog :: Integer -> Integer -> Bool
 isIntegralLog b k = (b ^ (integralLog b k) == k)

computes a logarithm and returns the value b^e such that b^e <= k, b,e,k are
 integers.

 integralLog :: Integer -> Integer -> Integer
 integralLog b k = (floor (logBase (fI b) (fI k)))



===============================================================================
divides
     returns true if k divides n, false otherwise

> divides :: Integer -> Integer -> Bool
> divides n k = k `mod` n  == 0


===============================================================================

radical
   doesn't quite work correctly

   returns the largest product of primes with exponent 1 which divides n, that
is: 
     a1    a2          an
x = p   * p   * ... * p   
     1     2           n

radical x = p * p  * ... * p
             1   2          n
 radical :: Integer -> Integer           
 radical k = foldr (*) 1 [k | k <- takeWhile (\a -> a <= logk 2 x) primes, x `divides` k]

===============================================================================

Euler phi / totient function

first, we'll define the alias, easy

 phi = totient

Now lets define the algorithm

 totient :: Integer -> Integer

 totient = 

===============================================================================

modExp is a simple implementation of modular exponentation by squaring

> modExp :: Integer -> Integer -> Integer -> Integer
> modExp x n m 
>    | n == 0            = 1
>    | odd n             = (x * (modExp ((x^2) `mod` m) ((n-1) `div` 2) m) `mod` m)
>    | even n            = (modExp ((x^2) `mod` m) (n `div` 2) m) `mod` m

===============================================================================

polygon numbers
	Here we define a general function for defining a n-gonal number of any size
a n-gonal number is defined as a number such that the same number of stones could be arranged
in a reguler n-gon. eg some triangle  (3-gonal) numbers are:

x 		1

x		3
xx

x
xx		6
xxx

x
xx
xxx		10
xxxx

...		(n(n+1))/2


A general formula for the kth n-gonal number is:
	2
(n - 2)k  - (n - 4)k
--------------------
        2

> -- returns the kth ngonal number
> polygonalNumber :: Integer -> Integer -> Integer
> polygonalNumber k n = ((n-2)*k^2 - (n-4)*k) `div` 2

> -- returns true if n is k-polygonal, else false
> isKpolygonal :: (RealFrac a, Floating a) => a -> a -> Bool
> isKpolygonal n k = isIntegral apoly  
>	where apoly = antipolygonal n k

> antipolygonal :: (RealFrac a, Floating a) => a -> a -> a
> antipolygonal n k = (2 * (n - 2*k + k^2)) / (k*(k-1)) 

> -- returns the nth triagonal number
> triagonalNumber :: Integer -> Integer
> triagonalNumber n = polygonalNumber n 3

> -- returns the nth square number
> squareNumber :: Integer -> Integer
> squareNumber = (^2)


===============================================================================


> applyClosure :: Ord a => (a -> a -> a) -> [a] -> [a]
> applyClosure f ls
>    | ls == closed           = ls
>    | otherwise              = applyClosure f closed
>    where closed = (removeDupes $ (ls ++ close ls))
>          close [] = []
>          close l@(x:xs) = [(f x y) | y <- l] ++ close xs
 
==============
seek
seek finds the first occurence satisfying p in a list ls

> seek :: (a -> Bool) -> [a] -> Maybe a
> seek _ [] = Nothing
> seek p (x:xs) 
>    | p x       = Just x
>    | otherwise = seek p xs

================================================================================

Arrow stuff

f <x> y = f xxx y for some x

> f <&> y = (f &&& f) y
> f <*> y = (f *** f) y

alternate f g x = (f <*>) &&& (g <x>) 
it's called alternate because it's a bit like alternating fractions:

let f,g = fst, snd

alternate fst snd ((a,b),(c,d) = ((a,c),(b,d)) 

which is a bit like:

a   c	  a   b
- = -  => - = -
b   d 	  c   d


> alternate f g = ((f <*>) &&& (g <*>) $)
> f <#> g = alternate f g 


