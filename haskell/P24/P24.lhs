> module P24 where

===============================================================================================
A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation
of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically,we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
================================================================================================


We can do this one of two ways, either by generating all the permutations and then picking the millionth (thats at least a million steps, not cool)

or we can save some time by figuring out how the digits move in each permutation. I prefer this method.


Lets think about how to represent a permutation. Really, we can think of this as a tree with the following conditions.

There is a Single Root node, at layer "-1"
If node N is at layer k, and C(N) is the children of node N, then all C(N) are at layer k+1
for all layers k, k > 0, and assuming we have |E| letters, then layer k haas E-k nodes.
Each Node N has a unique label within it's layer
if AC(n) is the set of all the parents of node n up to the root, then n is in E - AC(n). 

This gives us the following structure, with E = {0,1,2} 

Root 	|
	| 0 ----| 1 ----| 2
		|
		| 2 ----| 1
	|
	| 1 ----| 0 ----| 2
		|
		| 2 ----| 0
	|
	| 2 ----| 0 ----| 1
		|
		| 1 ----| 0

This helps us, we can now see the lexicographic structure, note in the example, we are given that
012,021,... is the ordering, and see here that we have the exact same ordering corresponding to the leftmost, next most leftmost, etc. So we can assume (rightly so) that the lexicographically _last_ value will be the rightmost path.

We also know that for |E| = n, there are n! different permutations, so we can use this to determine how many permutations live beneath a given element on the graph. 

How? It's a simply problem from first-year combinatorics. If I hold one element constant in a permutation, and I want to count all the permutations with that value constant, (and no repetitions), then I really just have all the unique permutations of n-1 symbols, with the constant symbol appended to it. This translates to the idea that moving to the left or right one node at layer k means that there are (|E| - k)! permutations I've skipped over. Proof as exercise to the reader.

So, we want to find the millionth permutation of |E|=10, well thats easy, because now it becomes a kind of logarithmic search, one we can do on paper. 

If |E| = 10, then we move by amounts of 9! at the first layer, so lets find the number closest to one million. 

9! = 362880, 9! * 3 > 10^6, so the first symbol is 2.

Now we move to the number of permutations beneath 2, thats (n-2)!, or
8! = 40320, 

however, we need to realize that now we're working towards not 10^6, but 10^6 - 2*9!, or 274240. so. Note something interesting, this is alot like the extended euclidean algorithm, consider, each time we're trying to find the following:

a / b = bq + r => q : (r / (b-1))

where / is our algorithm, which terminates when r = 0, so we can write this fairly quickly in haskell.


> findPermutation :: Integer -> Integer -> [Integer]
> findPermutation permToFind numSymbols 
> 	| numSymbols == 0 		= []
>	| permToFind > (fac numSymbols) = error "Permutation to find is out of range" 
>	| otherwise			= q : (findPermutation r (numSymbols - 1))
>	where (q,r) = permToFind `divMod` (fac (numSymbols - 1))

> fac 0 = 1
> fac n = n * fac (n-1)

however, this is _not the answer we want_ rather, it is the path which we must follow (proceeding
from the left towards the right to get the correct value, this is easily simulated by the following

> translatePerm :: [Integer] -> [Integer] -> [Integer]
> translatePerm [] _ = error "Dictionary not large enough"
> translatePerm _ [] = []
> translatePerm dict (x:xs) = x' : (translatePerm dict' xs)
>	where x' 	  = (choose dict x)
>	      dict' 	  = filter (/=x') dict
>	      choose ls x = ls !! ((fromInteger x)::Int) 	

> solvetwentyfour :: String
> solvetwentyfour = concatMap (show) 
>		  $ translatePerm [0..10]
>		  -- we need 10^6 - 1 because we start indexing at 0, not 1
>		  $ findPermutation (1000000 - 1) (10)

You can do this by hand, or with a calculator for bigger symbol sets. Whats really interesting, is to reverse this process and use it to calulate the permutation which corresponds to a particular word. Makes for an interesting code. However, that also requires that you remove the non-repeating symbol clause. Which makes the math a little different. 
 
