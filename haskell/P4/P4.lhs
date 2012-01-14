> module P4.P4 where
> import PELib.PELib

================================================================================
A palindromic number reads the same both ways. The largest palindrome made from
the product of two 2-digit numbers is 9009 = 91  99.

Find the largest palindrome made from the product of two 3-digit numbers.
================================================================================
I think its pretty obvious, this problem comes in three parts.

Part I) Generate all the products of three digit numbers
Part II) Create a Palindrome Predicate, and filter the list of all products for
palindromes
Part III) Take the max of the resulting list
================================================================================

==Part I, Generate them Products==

Easy, here it is

 tDPs :: [Integer]

> tDPs = removeDupes [n*m | n <- [111..999], m <- [111..999]]
                 

==Part II, Palindrome Predicates==

This is tougher, We need to reverse some numbers, so we want a function of type

> reverseNum :: Integer -> Integer

But how should we implement this? Well, we know we have a reverse for strings of
type:

reverse :: String -> String

And we know we have show, which is of type*

show :: Integer -> String

so we need to do (reverse . show) which has type

(reverse . show) :: Integer -> String

now we just neeed to turn the resultant back to an integer, so lets use read*

(read . reverse . show) :: Integer -> Integer

> reverseNum = (read . reverse . show)

So therefore the list of "palindromes" is

> palindromepairs :: [(Integer,Integer)]
> palindromepairs = removeDupes [(x, reverseNum x) | x <- tDPs]

but we still need to filter out all those which aren't palindromes so our real
list of palindromes is:

> palindromes :: [Integer]
> palindromes = ((map fst) . (filter (\(a,b) -> a == b))) palindromepairs

==Part III, Taking it to the max==

Now that we have a list of palindromes, we just need to take the maximum. Easy!

> solvefour :: Integer
> solvefour = maximum palindromes


*= show is technically not just of type Integer -> String, its of type: Show a
=> a -> String, but Show a => Integer a, ditto with read.
