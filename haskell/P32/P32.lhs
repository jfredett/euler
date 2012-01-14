> module P32 where

===============================================================================
The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing 
 multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can
 be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only 
 include it once in your sum.
===============================================================================

we know the following,

we're looking to find equations of the form:

a * b = c /\ (union d(a), d(b), d(c) = {1..9}) /\ (pairwise intersection d(a) d(b) d(c) = {})

where d(x) is the set of the digits of x, and d(a) = n, where n is a number, means that d(a) is the cardinality of
 d(a) in the normal case

We can quickly deduce the following condition from the first equation:

log(ab) = log(c) => log(a) + log(b) = log(c) => floor(log(a) + log(b)) = floor(log(c))

which, with the fact that 
floor(a + b) = floor(a) + floor(b) and floor(log(a)) = |d(a)| gives:
with log base 10

d(a) + d(b) = d(c)

we also know that: 
d(a) + d(b) + d(c) = 9

from conditions two and three

so then:

2*d(c) = 9

d(c) = 4.5 

which tells us d(c) is either 4 or 5
which further implies that d(a) + d(b) = 4,5 so (d(a),d(b)) will be one of the following : (1,4),(1,3),(2,3),(2,2), 
 wlog d(a) <= d(b)

so, our current set of conditions is as follows:

1 ab = c, 
2 d(a) union d(b) union d(c) = {1..9},
3 pairwise intersections of d(a),d(b),d(c) are null
4 d(c) = 4 \/ d(c) = 5
5 (d(a),d(b)) is one of [(1,4),(1,3),(2,3),(2,2)], wlog d(a) <= d(b)
6 d(a) + d(b) = d(c)
7 d(a) + d(b) = 9 - d(c)
8 a /= b /= c /= 1

8 was added on because it follows fairly quickly

So, lets try to shrink the size of the set in condition 5

lets assume that the d(LHS) = 4, then the maximum possible value one could have is either:

99x99 = c
9x999 = c

lets evaluate these 

99x99 = 9810, 
9x999 = 8991

these both have to few digits, which means that d(LHS) = 5 


so condition 5 becomes:

(d(a),d(b)) is one of [(1,4),(2,3)]

and the related condition 4 becomes:

d(c) = 4

we can derive a new condition based on 8, namely 8a:

if (d(a) = 1) then
	d(b) union d(c) contains 1


what would be very nice is to either show one or the other of the condition 5 cases to never work, and thereby prove 8a either useless or always true.

well, we know that the set of products corresponding to (2,3) = (4) is non-empty, because we have an example that fits that condition. now lets aim to show that the (1,4) case is uninhabited, we can test this directly:

> import Data.List (intersect, union, nubBy, sort)
> import Data.Function

this will just implement the first few conditions to check to make sure our triple is pandigital

> pandigitalTuple :: (Integer, Integer, Integer) -> Bool
> pandigitalTuple (a,b,c) = (null $ concat $ pairWiseIntersections) 
>			 && ((sort tupleunion) == "123456789")
>		where pairWiseIntersections = [x `intersect` y | x <- showntuple, y <- showntuple, x /= y]
>		      tupleunion = concat showntuple -- foldr union [] showntuple
>		      showntuple = [show a, show b, show c]

We have to test all a's on the range 2 to 9, and all b's on the range 1234 to 9876, without testing any a,b pairs with 
non-null intersections. And not testing b's with repeated digits

see below for why b < 10000/a

> test14Tuples = [(a,b,a*b) | a <- [2..9], b <-[1234..9876], nullIntersect a b, noRepeatDigits b, b < (10000 `div` a)]


> nullIntersect a b = null 
>		    $ (show a) `intersect` (show b)
> noRepeatDigits b = ((length . show) b) == (length $ (show b) `union` (show b))

Now we can test the testTuples to see if there exist any pandigital pairs

> condition14Test = filter pandigitalTuple test14Tuples

and we find that there are, in fact, two such tuples.

Now we can operate under the assumption that (d(a),d(b)) == (2,3)

We know that d(c) = 4, so b must satisfy:

b < 10000/a, 

	proof:
		let b = 10000/a, then ab = 10000 = c, d(c) = 5, which is a contradiction
		let b > 10000/a, then b = 10000/a + k, ab = 10000 + ak = c, since a,k > 0, d(c) >= 5, a contradiction
		therefore, b < 10000/a

this allows us to generate all possible tuples, and run our filter over them just like before:

> test23Tuples = [ (a,b,a*b) | a <-[12.. 98], b <-[123..987]
>		 , nullIntersect a b, noRepeatDigits a, noRepeatDigits b
>		 , b < (10000 `div` a)]
 
> condition23Test = filter pandigitalTuple test23Tuples


Now, these two tests don't take into account cases like the following:

let a /= b /= c /= d

ab = e = cd

That is, factorings of the same number that have this property.

we can account for this by doing the following:

> filteredResult = nubBy ((==) `on` (\(_,_,c) -> c)) 
>		 $ (condition14Test ++ condition23Test)

solve32 is then: 

> solvethirtytwo = sum $ map (\(_,_,c) -> c) filteredResult


