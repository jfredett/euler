>module P164 where

===============================================================================

How many 20 digit numbers n, with no leading 0, exist such that no three
consecutive digits of n have sum greater than 9?

===============================================================================

There are approximately 100,000,000,000,000,000,0000 numbers to check, if we 
work by brute force. However, there is an easier way. 

First, we can reduce to a simpler case, how many 3 digit numbers exist with that
property.

three digit number xyz is valid if x + y + z <= 9

therefore, we know if x = 1, then we have as many numbers as there are (integer) solutions
to y + z <= 8

similarly, if x = 2, then we have as many numbers as there are solutions to 
y + z <= 7; so how many solutions are there, (in general) to a + b <= c? 

we know we can separate the number of solutions into two parts, if Sol(f) is the
number of distinct solutions to f, then Sol(a+b <= c) = Sol(a+b < c) + Sol(a+b =
c) => Sol(a+b < c) + 2

Why two? Because we're talking about distinct, not unique, solutions, that is
1 + 2 = 3 and 2 + 1 = 3 are both valid, distinct solutions, and we need to count
them.

Not the hard case. Sol(a+b < c) is tough. We know that b < c - a, so if we take
a = c, then there are no solutions, for a = c - 1, then there is one solution,
and in general, there are n solutions for a = c - n. Since there are c-1 numbers
from 1 to c, then we have Sum(k, 1, c - 1) = k, which in general is n(n+1) / 2,
in our case, we have c(c-1)/2

So, the number of distinct solutions is c(c-1)/2 + 2 for each value of c, we
know there will be 9 values of c, so we have, 9*8/2 + 2 = 9*4 + 2 = 38

Therefore, there are 38 three digit values which satisfy the condition. Now lets
extend that to the 4 digit case. However, we need to account for the "leading 0"
condition. So we have 10(9)/2 + 2 -> 47.

wxyz. 

first, we know that there are 38 solutions for w + x + y <= 9, by the above
reasoning, (and noting that we cant have solutions with leading 0's, however,
now we need to find 
