> module P48.P48 where

================================================================================
Problem 48
18 July 2003

The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
================================================================================

Approach is simple, modular exp each of the components, modular sum.

> import PELib.PELib

> solve_forty_eight :: Integer
> solve_forty_eight = foldr (\a e -> (a + e) `mod` (10^10)) 0 [modExp x x (10^10) | x <- [1..1000]]
