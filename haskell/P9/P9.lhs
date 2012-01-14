> module P9.P9 where

================================================================================
A Pythagorean triplet is a set of three natural numbers, abc, for which,
a² + b² = c²

For example, 3² + 4² = 9 + 16 = 25 = 5².

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
================================================================================


Wow, Haskell might actually get in the way here, since theres no good way to
check to see if a number is an integer, I guess we'll have to invent one, one
way to do this is to have something which takes in anything of the type class
"num" and returns a boolean as if the number is equal to it's floor.

We'll do that Later.

Lets start by enumerating all the possible sums of a and b such that 

a + b + sqrt(a^2 + b^2) = 1000

> pykes :: [(Double, Double, Double)]
> pykes = filter (\(x,y,z) -> x+y+z == 1000) [(a,b,sqrt(a^2 + b^2)) | a <- [1..1000], b <- [1..1000]]

But hey, check this out, we know theres only one, so we don't even need to check
the type of anything, we just need to map everything down to a product, then
grab any element of the list, easy as pie

> solvenine :: Double
> solvenine = last (map (\(x,y,z) -> x*y*z) pykes)






