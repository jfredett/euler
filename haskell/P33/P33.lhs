> module P33 where

===============================================================================
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in 
attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is 
correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less 
than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common 
terms, find the value of the denominator.
===============================================================================

First, we know from the example one of the numbers we need, namely:

49/98, 

we can also easily notice the following:

16/64

and we notice that both of these reduce to a number of the form:

1/2^n

Lets enumerate the allowed fractions as pairs (num,denom)

(see below for detailed explainations of some of the filters)

> import Data.List (intersect, (\\), delete)
> import Data.Ratio

> possibleFracs = [ (n,d) | n <- [11..98], d <- [11..99]
>		  , n < d -- so the fraction is less than 1 in value
>		  , ((n `mod` 10) /= 0) && ((d `mod` 10) /= 0) -- so we don't have trivial cases 
>		  , (not . null) $ (show n) `intersect` (show d)
>		  ]


the first two filters are commented well enough. the third ensures that there is a digit available to cancel out. 

Now, since we know there is a value that is cancellable, lets cancel it, 

> data PairRatio = PR  { normal      :: (Integer, Integer) 
>		       , reducedrat  :: (Ratio Integer) 
>		       , magicredux  :: (Integer, Integer)
>		       , magicratio  :: (Ratio Integer)
>		       , magicWorked :: Bool		
>		       }
>	deriving (Eq, Show)

> mkPairRatio pa ra pb rb = (PR pa ra pb rb (ra == rb))
>			

> magicReducer :: (Integer, Integer) -> PairRatio
> magicReducer (a,b) = mkPairRatio (a,b) (a % b) (a', b') (a' % b')
>	where (a', b') = (aWCD , bWCD)
>	      commonDigit = head $ (show a) `intersect` (show b) 
>	      -- WCD = "Without common digit", they are strings
>	      aWCD = read $! delete commonDigit (show a) :: Integer
>	      bWCD = read $! delete commonDigit (show b) :: Integer

Easy enough, now we just run our magic reducer over the list of possibilities. Our "magicReducer" function notes when a ratio and it's magic reduction work, via the magicWorked parameter, we'll use that in the solve33 function later

> magicReducedFracs = map magicReducer possibleFracs

now we want just the ones that pass

> filteredReducedFracs = filter magicWorked magicReducedFracs

Now we just need to compute the value of the denominator of the product of these fractions, simple enough

> solvethirtythree = (denominator . product . map reducedrat) filteredReducedFracs
>		   


-- this should get put in PELib at some point

> ppList [] = do
>	putStrLn "done"
> ppList (x:xs) = do
>	putStrLn (show x)
>	ppList xs



