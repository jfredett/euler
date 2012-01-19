> module P34.P34 where

Problem 34
===============================================================================
January 2003

145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their
digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included. 
===============================================================================

> import Data.List (sort)
> import Data.Set (fromList, toList)


> digits :: Integer -> [Integer]
> digits = reverse . digits' 
>   where digits' x 
>           | x < 10    = [x]
>           | otherwise = (x `mod` 10) : digits' (x `div` 10)

> factorial 0 = 1
> factorial 1 = 1
> factorial x = product [2..x]

By analysis, we can show that -- unless the number is less than 7 digits, it
cannot be the sum of it's digits' factorials.

Given this, we can simply perform the following function on about 10 million
numbers, a la

> isFactorialSum :: Integer -> Bool
> isFactorialSum x = (sum . map factorial . digits $ x) == x

> solve_thirty_four_slow = sum [x | x <- [1..10^9], isFactorialSum x]

However, this is very slow. We should be able to do this more effectively.

Firstly, we can solve to a better resolution the upper bound. 

The upper bound is calculated based on the following principle. At some point,
the size of the number being calculated will be bigger than the possible sum of
it's factorial digits (which I will call "figits"). This is governed by the
equation:

        9! * k + 1 < 10^k 
    =>  362880k + 1 < 10^k

We can write a simple convergence solver for this, as follows:

> convergence old = convergence' old old 1

> convergence' old new adj 
>   -- old was too big and this was too big, don't change adj
>   | guess old && guess new         = new : convergence' new (new + adj) adj
>   -- turning point, decrease adj
>   |  guess old && not (guess new)  
>   || not (guess old) && guess new  = new : convergence' new new (adj / 2)
>   -- both small, subtract adj, don't adjust
>   | not (guess old || guess new)   = new : convergence' new (new - adj) adj
>   where guess x = (factorial . fromInteger $ 9) * x + 1 > 10**(x+1)

This converges to a value of `5.2851875`, That's the value of `k`, we are
concerned with `k+1`, so we need to search to `6.2851875`, which shaves off
about 8 million values. We can try this again with the new upper bound.

Note that 10**6.2851875 ~ 8071643 (it's a bit lower than that)

> solve_thirty_four_less_slow = sum [x | x <- [145..8071643], isFactorialSum x]

This, however, still takes too long. We can continue to analyze away some more
cases.

Notice -- every factorial is even except for 0 and 1. So, any odd number which
is the sum of it's figits will necessarily contain a 0 or a 1. So we can
exclude numbers based on this predicate

--> isFigitable x = even x || (0 `elem` (digits x) || 1 `elem` (digits x)) 

that'll get us down to ~7 million values. We can also make a stronger statement
-- it is only figitiable if it contains an odd number of 0's and 1's

> isFigitable x = even x 
>              || (odd.length $ [d | d <- digits x, d `elem` [0,1]])

This gets us to just under 6 million.

Next, consider if a value contains a digit, but is less than that digit
factorial -- eg, `55` contains a `5`, but is less than `5!`, this provides a
whole host of reductions to do. 

> isBigEnough x = all (isBigEnough' x) [0..9]
>   where isBigEnough' x d 
>            | d `elem` (digits x) = factorial d < x
>            | otherwise           = True

However, this gives us _far_ too small of a reduction (even though it applies to
many things, most of those things aren't filtered.

This indicates a problem -- there's really no way to reduce this much further.
Rather, we need a new approach.

--------

Using our previous result, we know that there are at most 7 digits (since the
possibly figitiable maximum value is somewhere shortly north of 8 million, a 7
digit number). 

We know that what we really want is a sort of "set" of digits -- particularly,
we want the number of sets of digits, without regard to order, and with regard
to repetition. Eg `[1,1,2]` is a valid set, to which `[1,2,1]` and `[2,1,1]` is
equivalent. Essentially we want to chose values from a list like:

    [ [ (0,0), (1,0), (2,0) ... ],
      [ (1,0), (1,1) ... ],
      ...
      [ (7,0) ... (7,8) ] ] 

where the first digit indicates the position of the digit, and the latter digit
at that position. We need to choose one element from each set, but ignoring
ordering -- so [(0,1), (1,1), (2,2)] should equal [(0,1), (1,2), (2,1)].
Processing 8 million subsets of these is far less intensive, so, hopefully it
will be fast enough to just get all of the values, filter the duplicates, and
then process the results to see which are figitals. 

> data Figit = Figit Integer Integer -- first is place, second is value
>     deriving (Eq, Show)
> -- this will need a new imp of Eq, so it's a newtype
> newtype Figital = Figital [Figit]
>     deriving Show

> mkFigit :: Integer -> Figital
> mkFigit x = Figital . map (uncurry Figit) $ (zip [0..] (digits x))


> value :: Figit -> Integer
> value (Figit _ v) = v
> pos :: Figit -> Integer
> pos (Figit i _) = i

> toDigital :: Figital -> Integer
> toDigital (Figital [])     = 0
> toDigital (Figital (x:xs)) = 10^(pos x) * (value x) + (toDigital . Figital $ xs) 

> instance Eq Figital where
>    Figital ls == Figital ms = (sort . map value $ ls) == (sort . map value $ ms)
> instance Ord Figital where
>    fs <= ms = toDigital fs <= toDigital ms

Now we can compare (and importantly, reduce) a list of possible values to a
minimum set. We know that the sum of these digits will be unique, so we don't
have to worry about not counting enough.

In fact, building the figitals for the 8 million digits is lightning fast in the
interpreter, now lets build the uniqification filter. Fortunately, this is done
for us! It's just called Data.Set!

> solve_thirty_four_via_set = fromList [mkFigit x | x <- [145..8071643], isFigitable x]



