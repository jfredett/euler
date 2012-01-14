> module P28 where

===============================================================================
Starting with the number 1 and moving to the right in a clockwise direction a 
5 by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of both diagonals is 101.

What is the sum of both diagonals in a 1001 by 1001 spiral formed in the same way?
===============================================================================

Let's consider the distance between two corners in an odd edge length square

D(n) = 2n -1 for all odd n

the Perimiter of a odd-square is:

P(n) = 4*D(n) + 4 = 4(2n -1) + 4 = 8n  forall odd n 

A(n) = n^2

Now lets consider the upper right diagonal, We'll number them like the cartesian quadrants, I,II,III, and IV. 

Consider I, we notice in this short example that it seems to be equal to n^2. and indeed, it is, this is because as we count around the spiral, it is always the last position in a spiral-square to be filled. So, we can write the following function to generate a stream for I

These are transformers, taking the nth natural number, and turns it into the nth term of the
corresponding series

the interesting bit is that, since we know the distance between corners for a odd-square, we can determine the others as just being a constant away.

Hence:

> i,ii,iii,iv, cornersum :: Integer -> Integer 
> i   n = n*n
> ii  n = (i n) - (n - 1)
> iii n = (ii n) - (n - 1)
> iv  n = (iii n) - (n - 1)  
> cornersum n = (i n) + (ii n) + (iii n) + (iv n) - overcount
>	where overcount = if n == 1 
>			then 3 
>			else 0

so, now we can just find the sum of the cornersum, However, all of these function take the length
of the side of the square, since we're only concerned with odd sides, then i - iv and cornersum only work on odd arguments. (In reality, (i,iii) and (ii,iv) are the same functions, shifted once to the left/right. i on an even value n = iii on an odd value (n-1)). 

so our solution is as follows:

> solvetwentyeight :: Integer 
> solvetwentyeight = sum $ map cornersum [1,3..1001] 
