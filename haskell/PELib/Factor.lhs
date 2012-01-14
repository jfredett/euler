> {-# OPTIONS -fglasgow-exts #-}
> module PELib.Factor where

===============================================================================
Factor provides:
     factor :: Integer -> [Integer] 
     --factors an integer into its components
     sigma :: Integer -> Integer -> Integer
     --sum of divisors^k function
     divisors :: Integer -> [Integer]
     --all divisors (not just prime ones) of a number
     legendre :: Integer -> Integer -> Maybe Integer
     -- returns the lengendre symbol of a number
     jacobi :: Integer -> Integer -> Integer
     -- returns the jacobi symbol of a number
     kronecker :: Integer -> Integer -> Integer
     -- returns the kronecker symbol of a number
     quadraticResidue :: Integer -> Integer -> Integer
     --                              2
     -- returns a value x such that x  = a (mod p) where a and p are the
     --  arguments, p is prime greater than 2. 
     


TODO:

NOTE:

Move Sigma, Divisor, etc from NumberClasses to here.



===============================================================================

> import PELib.PELib
> import Test.QuickCheck

=======================================
The Legendre Symbol
http://en.wikipedia.org/wiki/Lengendre_symbol/

The Legendre Symbol is used in connection with factorization and quadratic
residues, it is used in the Shanks-Tonelli quadratic residue algorithm

> legendre :: Integer -> Integer -> Maybe Integer
> legendre a p
>    | (not . isPrime) p = Nothing
>    | p `divides` a     = Just 0
>    | a == 1            = Just 1
>    | otherwise         = sym (modExp a ((p-1)`div`2) p)
>    where
>         sym k
>           | k == (p - 1) = Just (-1) 
>           | otherwise    = Just k 


=======================================
The Jacobi Symbol
http://en.wikipedia.org/wiki/Jacobi_symbol/

A generalization of the Legendre Symbol.

 jacobi :: Integer -> Integer -> Integer

=======================================
The Kronecker Symbol

A further generalization of the Jacobi Symbol

 kronecker :: Integer -> Integer -> Integer

=======================================
Shanks-Tonelli, or Quadratic Residue
http://en.wikipedia.org/wiki/Shanks-Tonelli_algorithm 

Finds a k such that
  2 
 k  = n  (mod p)

> -- this might be returning wrong answers, I'm not sure,
> -- for instance, quadraticResidue 2 19 returns 13, which, when squared
> -- mod 19, returns 17, which is -2 mod 19, which is a little weird. i marked
> -- the parts that could be broken
> quadraticResidue :: Integer -> Integer -> Maybe Integer
> quadraticResidue n p 
>    | (not . isPrime) p         = Nothing
>    | p == 2                    = Just (n `mod` p) 
>    -- might be wrong
>    | p `mod` 4 == 3            = Just (modExp n ((p+1) `div` 4) p)
>    -- seems to be hanging
>    | (legendre n p) == Just 1  = Just (shanksTonelli n p)
>    | otherwise                 = Nothing

> shanksTonelli n p
>    | (s==1)    = r 
>    | otherwise = shanksTonelli' r
>    where 
>         s = removeTwos (p-1)
>         q = (p-1) `div` (2^s)
>         w = chooseLegendre 2 p                   
>         r = modExp n ((q+1) `div` 2) p
>         v = modExp w q p 
>         ninverse = modExp n (p-2) p
>         shanksTonelli' r
>              | (i == 0)     = r
>              | otherwise    = shanksTonelli' r'
>              where
>                   iterList = iterate (\x -> modExp x 2 p) ((r^2)*ninverse)
>                   iter = [(x,y) | x<-iterList | y<-[0,1..]]
>                   f z = case z of 
>                             Just z -> z
>                             Nothing -> 0
>                   -- returns a Maybe a, need to fix it
>                   i = f ((seek (\(x,y) -> x==1) iter) >>= (Just . snd))
>                   r' = ((r*(modExp v (2^(s-i-1)) p)) `mod` p)
>
>         removeTwos k 
>              | even k    = 1 + removeTwos (k `div` 2)
>              | otherwise = 0
>         chooseLegendre x y
>              | legendre x y == Just (-1)  = x
>              | otherwise                  = chooseLegendre (x+1) y
>         
>              





