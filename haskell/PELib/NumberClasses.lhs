> module PELib.NumberClasses where

===============================================================================
This library will contain algorithms for determining some "classes" of numbers,
where a class of numbers is defined as the set of numbers having some property.

Currently, there is support for classification by:

Divisibility-based number classes:
   see http://en.wikipedia.org/wiki/Prime_number

===============================================================================

> import PELib.PELib

=== NicoIdx && NicomachusSymbol ===
NicoIdx is either Abundant, Perfect, or Deficient, it is named after Nicomachus
(c 100AD), the mathematician who first introduced the classification. 

> data NicoIdx = EAbundant | EPerfect | EOther | One | Zero
>         deriving (Eq, Read, Show, Enum, Bounded) 

> nicomachusSymbol :: Integer -> NicoIdx
> nicomachusSymbol k 
>    | 0 == k  = Zero
>    | 1 == k  = One
>    | s == k  = EPerfect 
>    | s < k   = EOther
>    | s > k   = EAbundant
>    where s = ((sigma 1 k) - k)

some handy aliases / predicates

> isPerfect :: Integer -> Bool
> isAbundant :: Integer -> Bool
> isDeficient :: Integer -> Bool

> isPerfect k = nicomachusSymbol k == EPerfect
> isAbundant k = nicomachusSymbol k == EAbundant
> isDeficient k = nicomachusSymbol k == EOther

a minimally abundant number is a number x such that if x = kp, s = sigma(1,x),
that x/k > s-k, every abundant number is a multiple of a minimally abundant
number.

> minAbundant :: Integer -> Bool
> minAbundant k = isAbundant k 
>               && (none [isAbundant (k `div` d) | d <- divs])
>               where divs = tail $ properDivisors k
>                     none = (not . or)

findMinAbundant, given an abundant number, finds the minimal abundant number it
is a multiple of.

==>maybe later.


========== sigma / divisors ===========
divisors returns a list of divisors, sigma is the "sum of divisors function"
which takes an exponent and a number, and returns:

            ___   x
 s(x,n) =   \    p 
            /__  
            p|n

> divisors :: Integer -> [Integer]
> divisors k 
>    | k == 1       = [1]
>    | isPrime k    = [1,k]
>    | otherwise    = applyClosure closefunc (firstFactor : remnant : divisors remnant)
>    where
>         firstFactor = head $ dropWhile (not . (`divides` k)) primes
>         remnant = k `div` firstFactor      
>         closefunc a b = (a*b) `gcd` k

heres sigma

> sigma :: Integer -> Integer -> Integer
> sigma x k = sum [y^x | y <- divisors k] 

Now lets define a few handy aliases

> improperDivisors :: Integer -> [Integer]
> improperDivisors = divisors

> properDivisors :: Integer -> [Integer]
> properDivisors = (init . divisors)

=============other classes==============
        
EPerfect, EAbundant, and EOther are extended versions of the Nicomachus
symbol types

> data EPerfect = Almost | Quasi | Pluperfect Integer
>               | Hyperperfect Integer | Unitary | Semi | PrimitiveSemi 
>               | Perfect | NotPerfect
>               deriving (Show, Read, Eq)

> data EAbundant = Highly | Super | Minimal | Collosal | HighlyComposite 
>                | SuperiorComposit | Abundant | NotAbundant
>                deriving (Show, Read, Eq, Enum, Bounded)

> data EOther = Weird | Amicable | Friendly | Sociable
>             | Solitary | Sublime | HarmonicDivisor | Frugal
>             | Equidigital | Extravagant | Deficient | Undef
>             deriving (Show, Read, Eq, Enum, Bounded)


> perfectType :: Integer -> EPerfect
> perfectType k 
>    | almostTest             = Almost
>    | quasiTest              = Quasi
>    | isPerfect k            = Perfect
>    | pluPerfectTest         = Pluperfect pluPerfectIndex
>    | hyperPerfTest          = Hyperperfect hyperPerfIndex
>    | otherwise              = NotPerfect
>    where s = (sigma 1 k)
>          almostTest = (s==(2*k)-1)
>          quasiTest = (s==(2*k)+1)
>          pluPerfectIndex = (s `div` k)
>          pluPerfectTest = (pluPerfectIndex*k == 2*k)
>          hyperPerfNum = (s - k - 1)
>          hyperPerfIndex = (k - 1) `div` hyperPerfNum
>          hyperPerfTest = hyperPerfIndex * hyperPerfNum == k-1 


