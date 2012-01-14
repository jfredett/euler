> module P67.P67 where
> import P18.P18
> import P67.BigTreeList


===============================================================================
Problem 67
09 April 2004

By starting at the top of the triangle below and moving to adjacent numbers on
the row below, the maximum total from top to bottom is 23.

3
7 5
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.

Find the maximum total from top to bottom in P67.BigTreeList
 a 15K text file containing a triangle with one-hundred rows.

NOTE: This is a much more difficult version of Problem 18. It is not possible to
try every route to solve this problem, as there are 299 altogether! If you could
check one trillion (10^12) routes every second it would take over twenty billion
years to check them all. There is an efficient algorithm to solve it. ;o)
===============================================================================

We imported P18 because we have the solution already, and P67.BigTreeList
contains a list version of the 15k text file. We then simply run rollUpTree on
the list, and get the answer

> solvesixtyseven :: Integer
> solvesixtyseven = rollUpTree (reverse bigTreeList)
                        


