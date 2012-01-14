> module P30 where

===============================================================================
Surprisingly there are only three numbers that can be written as the sum of fourth
 powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 1^4 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
===============================================================================


First of all, it's fairly reasonable to assume that there are not many numbers equal to the 
sum of the fifth power of there digits.

Lets first try to count the unique sums of fifth powers, or really, unique sums of nth powers.

We know that the largest number representable with a sum of fifth powers is the arbitrary sum of
9^5. and the smallest representable such number, is 11, because it must be a sum. Lets focus on coming up with some harder bounds on that range.

We'll start by looking for the lowest representable number, or LRN. 

We know that the LRN obeys this identity:

f(k) = Sum(a_i * 10^i, 0 <= i <= n) = Sum(a_i^5, 0 <= i <= n) for some series a_1, a_2, ... a_n, where each a is on the range of integers [0,9]. What we need here is a way to express "take the 
sum of the fifth powers of the digits" in a way that does not involve talking about the digits directly. Ideally, we want to have a completely algebraic way of saying this.


