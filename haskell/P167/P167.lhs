> module P167 where
> import Data.List

==============================================================================
For two positive integers a and b, the Ulam sequence U(a,b) is defined by
U(a,b)1 = a, U(a,b)2 = b and for k > 2, U(a,b)k is the smallest integer greater
than U(a,b)(k-1) which can be written in exactly one way as the sum of two
distinct previous members of U(a,b).

For example, the sequence U(1,2) begins with
1, 2, 3 = 1 + 2, 4 = 1 + 3, 6 = 2 + 4, 8 = 2 + 6, 11 = 3 + 8;
5 does not belong to it because 5 = 1 + 4 = 2 + 3 has two representations as the
sum of two previous members, likewise 7 = 1 + 6 = 3 + 4.

Find Sum(U(2,2n+1))_1e11) for 2 <= n <= 10.
===============================================================================

First, lets implement a way to generate these generalized Ulam numbers.

We start by creating a general function which will take two ulam numbers and
return the corresponding Ulam sequence.

> type UlamSequence a = [a]

> ulam 0 ls = ls
> ulam len ls = ulam (len - 1) (ulamNext ls)


Now, lets get to the meat of the operation, this function will take an Ulam
sequence, and produce the next number in that sequence.

> ulamNext ls = ls ++ [nextelem]
>    where nextelem = head -- head == minimum of sorted lists 
>                   -- get rid of the lengths
>                   $ map fst
>                   -- choose unique values
>                   $ filter ((==1) . snd)
>                   -- create all ulam pairs
>                   $ [(head x, length x)
>                     | x <- collect [ x + y 
>                                    | x <- ls, y <- ls
>                                    , x /= y, x < maximum ls]
>                     , head x > maximum ls
>                     ]


> collect :: Ord a => [a] -> [[a]]
> collect = (group . sort)


We can, however, also do this in a more general way, if we calculate U(a,b) in
the general case, we find that what we are looking for at each turn is a
combination of two vectors from the previous incarnation of U(a,b) that is
pairwise linearly indepented from all the other vectors. That is

(1,0)  = a          = 1
(0,1)  = b          = 2
(1,1)  = a + b      = 3
(2,1)  = 2a + b     = 4
(2,2)  = 2a + 2b    = 6
(2,3)  = 2a + 3b    = 8
.
.
.

So great, lets just create a 2-vector type and plug that into the ulam function.

Heres the code:

First the type

> data UlamVector a = UV [a] deriving Show

Numerability

> instance (Enum a, Eq a, Num a) => Num (UlamVector a) where
>    (UV vs) + (UV ws) = UV (zipWith (+) vs ws)
>    (UV vs) * (UV ws) = UV (zipWith (*) vs ws) -- its just pairwise product
>    (UV vs) - (UV ws) = UV (zipWith (-) vs ws)
>    abs (UV vs)       = UV (map (abs) vs)
>    signum (UV vs)    = UV [] -- we're not going to use it anyway
>    fromInteger _     = UV []

> instance (Num a, Eq a, Enum a) => Eq (UlamVector a) where
>    vs == ws = (ulamRectify' vs) == (ulamRectify' ws)

> instance (Num a, Ord a, Enum a) => Ord (UlamVector a) where
>    compare (UV vs) (UV ws) 
>         | rectVs <  rectWs = LT
>         | rectVs == rectWs = EQ
>         | rectVs >  rectWs = GT
>         where rectVs = (ulamRectify' (UV vs)) 
>               rectWs = (ulamRectify' (UV ws))
>    

> ulamRectify :: Num a => UlamVector a -> [a] -> a
> ulamRectify (UV vs) ws = sum $ zipWith (*) vs ws

> ulamRectify' :: (Num a, Enum a) => UlamVector a -> a
> ulamRectify' (UV ws) = ulamRectify (UV ws) [(toEnum 1)..(toEnum $ length ws)]

====
What with all that done, we want to find a good way to sum all the series at
once, then we can look for optimizations to make it go fast.

first, heres code to sum the full series using the naive method.

> ulamSum :: Integer -> Integer
> ulamSum numPlaces = sum
>                   $ map sum 
>                   $ [(ulam numPlaces [2,2*(x+1)]) | x <-[2..10]]

heres how to do it using the vector method from earlier

-> ulamSum' :: Integer -> Integer

> ulamSum' numPlaces = id --sum
>                    $ id --map sum
>                    $ [map t generalSeq | t <- transformers] 
>                where generalSeq = ulam numPlaces [UV [1,0], UV [0,1]]
>                      transformers = [((flip $ ulamRectify) [2,2*(x+1)]) | x <- [2..10]]    

















